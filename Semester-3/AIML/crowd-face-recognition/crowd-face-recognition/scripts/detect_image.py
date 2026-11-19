"""Day 1: detect every face in a crowd image and save an annotated copy.

Examples
  python scripts/detect_image.py --image data/images/crowd.jpg
  python scripts/detect_image.py --image data/images/crowd.jpg --tile        # small faces
  python scripts/detect_image.py --sample                                     # built-in test photo
  python scripts/detect_image.py --image crowd.jpg --compare                  # normal vs tiled
"""
import argparse
import sys
import time
from pathlib import Path

import cv2

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
from src import config  # noqa: E402
from src.detector import FaceDetector, draw_detections  # noqa: E402


def load_image(args):
    if args.sample:
        from insightface.data import get_image
        return get_image("t1"), "sample_t1"
    img = cv2.imread(args.image)
    if img is None:
        sys.exit(f"Could not read image: {args.image}")
    return img, Path(args.image).stem


def run(det, img, tile):
    det.detect(img, tile=tile)  # warm-up (first call is slow)
    t0 = time.perf_counter()
    boxes, scores, kpss = det.detect(img, tile=tile)
    ms = (time.perf_counter() - t0) * 1000
    return boxes, scores, kpss, ms


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--image", help="path to an image")
    p.add_argument("--sample", action="store_true", help="use InsightFace's built-in group photo")
    p.add_argument("--tile", action="store_true", help="tiled detection for tiny faces")
    p.add_argument("--compare", action="store_true", help="run normal AND tiled, save both")
    p.add_argument("--det-size", type=int, default=config.DET_SIZE)
    p.add_argument("--show", action="store_true", help="open a window with the result")
    args = p.parse_args()
    if not args.image and not args.sample:
        p.error("give --image PATH or --sample")

    img, stem = load_image(args)
    det = FaceDetector(det_size=args.det_size)
    print(f"Device: {det.device} | image {img.shape[1]}x{img.shape[0]}")

    modes = [False, True] if args.compare else [args.tile]
    config.OUTPUT_DIR.mkdir(exist_ok=True)
    for tile in modes:
        boxes, scores, kpss, ms = run(det, img, tile)
        name = "tiled" if tile else "normal"
        out = draw_detections(img, boxes, scores, kpss)
        path = config.OUTPUT_DIR / f"{stem}_{name}.jpg"
        cv2.imwrite(str(path), out)
        print(f"[{name:>6}] faces: {len(boxes):3d} | time: {ms:7.1f} ms | saved -> {path}")
        if args.show:
            cv2.imshow(f"{name}", out)
    if args.show:
        cv2.waitKey(0)


if __name__ == "__main__":
    main()
