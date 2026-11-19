# Face Recognition in Crowded Area Video

Detect and identify people in crowded videos in real time, with an interactive live dashboard.

**Pipeline:** frame → face detection (SCRFD) → alignment → ArcFace embedding → match with gallery → tracking (ByteTrack) → dashboard

## Folder structure

```
crowd-face-recognition/
├── src/            # core code (detector, recognizer, tracker)
│   ├── config.py   # all settings in one place
│   └── detector.py # face detector with tiling for tiny faces
├── scripts/        # runnable scripts (one per task)
├── app/            # live demo dashboard (Day 4 onward)
├── data/
│   ├── gallery/    # one folder per enrolled person: gallery/Vivek/1.jpg ...
│   ├── videos/     # crowd test videos
│   └── images/     # crowd test images
├── outputs/        # annotated results (auto-created)
├── notebooks/      # experiments, plots
└── reports/        # report material, result images
```

## Setup (Windows / Mac / Linux)

Requires **Python 3.10–3.12** (3.13 also works).

```bash
cd crowd-face-recognition
python -m venv venv
# Windows:   venv\Scripts\activate
# Mac/Linux: source venv/bin/activate
pip install -r requirements.txt
python scripts/check_env.py
```

**NVIDIA GPU?** Run `pip uninstall onnxruntime` then `pip install onnxruntime-gpu`. The code picks the GPU automatically.

**Windows: `insightface` fails to install?** Install "Microsoft C++ Build Tools" (Desktop development with C++), then run `pip install insightface` again.

The first run downloads the face models (~280 MB) to `~/.insightface/models/`.

## Day 1: face detection

```bash
python scripts/detect_image.py --sample --compare           # built-in test photo
python scripts/detect_image.py --image data/images/crowd.jpg --compare
```

`--compare` runs normal and tiled detection and saves both to `outputs/`.

| Mode | When to use | Speed |
|---|---|---|
| normal | live video, faces medium/large | fast |
| `--tile` | wide crowd shots, tiny faces | slower (~10–15×) |

**Day 1 result** (synthetic crowd, 2256×1560, 384 tiny faces, CPU):

| Mode | Faces found | Time |
|---|---|---|
| normal | 108 | 0.19 s |
| tiled | 384 (all) | 2.8 s |

See `reports/day1_tiny_crowd_normal.jpg` and `reports/day1_tiny_crowd_tiled.jpg`.

## Ethics

Only people who have given consent are enrolled in `data/gallery/`. The dashboard will include a privacy mode that blurs unknown faces.
