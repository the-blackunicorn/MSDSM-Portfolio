"""Central settings for the project. Change values here, not inside scripts."""
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
DATA_DIR = ROOT / "data"
GALLERY_DIR = DATA_DIR / "gallery"      # one sub-folder per enrolled person
VIDEO_DIR = DATA_DIR / "videos"
IMAGE_DIR = DATA_DIR / "images"
OUTPUT_DIR = ROOT / "outputs"

# InsightFace model pack: "buffalo_l" (accurate) or "buffalo_s" (faster, for weak laptops)
MODEL_PACK = "buffalo_l"

# Detection
DET_SIZE = 640            # detector input size (pixels). Higher = finds smaller faces, slower
DET_THRESHOLD = 0.5       # minimum detection confidence
MIN_FACE_SIZE = 12        # ignore faces smaller than this (pixels, shorter side)

# Tiling (helps with tiny faces in crowds)
TILE_SIZE = 640
TILE_OVERLAP = 0.25
NMS_IOU = 0.4
