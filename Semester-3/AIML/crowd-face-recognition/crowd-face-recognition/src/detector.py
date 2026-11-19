"""Face detector for crowded scenes (SCRFD via InsightFace), with optional tiling.

Tiling: a crowd frame is split into overlapping tiles, each tile is run through the
detector at full resolution, and boxes are merged with non-max suppression. This finds
small, distant faces that are lost when the whole frame is shrunk to 640x640.
"""
from __future__ import annotations

import numpy as np
import onnxruntime as ort
from insightface.app import FaceAnalysis

from src import config


def pick_providers() -> list[str]:
    """Use the GPU automatically if onnxruntime-gpu and CUDA are available."""
    available = ort.get_available_providers()
    if "CUDAExecutionProvider" in available:
        return ["CUDAExecutionProvider", "CPUExecutionProvider"]
    return ["CPUExecutionProvider"]


def nms(boxes: np.ndarray, scores: np.ndarray, iou_thr: float) -> list[int]:
    """Standard non-max suppression. boxes: (N,4) x1,y1,x2,y2."""
    if len(boxes) == 0:
        return []
    x1, y1, x2, y2 = boxes.T
    areas = (x2 - x1) * (y2 - y1)
    order = scores.argsort()[::-1]
    keep = []
    while order.size:
        i = order[0]
        keep.append(int(i))
        xx1 = np.maximum(x1[i], x1[order[1:]])
        yy1 = np.maximum(y1[i], y1[order[1:]])
        xx2 = np.minimum(x2[i], x2[order[1:]])
        yy2 = np.minimum(y2[i], y2[order[1:]])
        inter = np.clip(xx2 - xx1, 0, None) * np.clip(yy2 - yy1, 0, None)
        iou = inter / (areas[i] + areas[order[1:]] - inter + 1e-9)
        order = order[1:][iou <= iou_thr]
    return keep


class FaceDetector:
    def __init__(self, det_size: int = config.DET_SIZE, det_thresh: float = config.DET_THRESHOLD):
        self.providers = pick_providers()
        # Detection-only: skips recognition/landmark models, so it is fast.
        self.app = FaceAnalysis(name=config.MODEL_PACK, allowed_modules=["detection"],
                                providers=self.providers)
        ctx = 0 if self.providers[0].startswith("CUDA") else -1
        self.app.prepare(ctx_id=ctx, det_size=(det_size, det_size), det_thresh=det_thresh)
        self.det = self.app.det_model
        self.det_size = det_size

    @property
    def device(self) -> str:
        return "GPU (CUDA)" if self.providers[0].startswith("CUDA") else "CPU"

    def _detect(self, img: np.ndarray):
        bboxes, kpss = self.det.detect(img, input_size=(self.det_size, self.det_size))
        if kpss is None:
            kpss = np.zeros((len(bboxes), 5, 2), dtype=np.float32)
        return bboxes, kpss

    def detect(self, img: np.ndarray, tile: bool = False):
        """Returns (boxes (N,4), scores (N,), landmarks (N,5,2))."""
        if not tile:
            bboxes, kpss = self._detect(img)
        else:
            bboxes, kpss = self._detect_tiled(img)
        if len(bboxes) == 0:
            return np.zeros((0, 4)), np.zeros(0), np.zeros((0, 5, 2))
        boxes, scores = bboxes[:, :4], bboxes[:, 4]
        w = boxes[:, 2] - boxes[:, 0]
        h = boxes[:, 3] - boxes[:, 1]
        ok = np.minimum(w, h) >= config.MIN_FACE_SIZE
        return boxes[ok], scores[ok], kpss[ok]

    def _detect_tiled(self, img: np.ndarray):
        H, W = img.shape[:2]
        ts = config.TILE_SIZE
        step = int(ts * (1 - config.TILE_OVERLAP))
        all_b, all_k = [], []

        # 1) Whole frame once, for large/near faces that tiles might cut in half.
        b, k = self._detect(img)
        all_b.append(b); all_k.append(k)

        # 2) Overlapping tiles at native resolution, for small/far faces.
        if max(H, W) > ts:
            ys = list(range(0, max(H - ts, 0) + 1, step)) or [0]
            xs = list(range(0, max(W - ts, 0) + 1, step)) or [0]
            if ys[-1] + ts < H: ys.append(H - ts)
            if xs[-1] + ts < W: xs.append(W - ts)
            for y in ys:
                for x in xs:
                    crop = img[max(y, 0):y + ts, max(x, 0):x + ts]
                    b, k = self._detect(crop)
                    if len(b):
                        b = b.copy(); k = k.copy()
                        b[:, [0, 2]] += max(x, 0); b[:, [1, 3]] += max(y, 0)
                        k[:, :, 0] += max(x, 0); k[:, :, 1] += max(y, 0)
                        all_b.append(b); all_k.append(k)

        bboxes = np.concatenate(all_b) if all_b else np.zeros((0, 5))
        kpss = np.concatenate(all_k) if all_k else np.zeros((0, 5, 2))
        keep = nms(bboxes[:, :4], bboxes[:, 4], config.NMS_IOU)
        return bboxes[keep], kpss[keep]


def draw_detections(img: np.ndarray, boxes, scores, kpss=None) -> np.ndarray:
    import cv2
    out = img.copy()
    for i, (b, s) in enumerate(zip(boxes, scores)):
        x1, y1, x2, y2 = b.astype(int)
        cv2.rectangle(out, (x1, y1), (x2, y2), (0, 220, 0), 2)
        cv2.putText(out, f"{s:.2f}", (x1, max(y1 - 4, 10)), cv2.FONT_HERSHEY_SIMPLEX,
                    0.45, (0, 220, 0), 1, cv2.LINE_AA)
        if kpss is not None:
            for (px, py) in kpss[i].astype(int):
                cv2.circle(out, (px, py), 1, (0, 0, 255), -1)
    label = f"Faces: {len(boxes)}"
    cv2.rectangle(out, (0, 0), (170, 34), (0, 0, 0), -1)
    cv2.putText(out, label, (8, 24), cv2.FONT_HERSHEY_SIMPLEX, 0.8, (255, 255, 255), 2)
    return out
