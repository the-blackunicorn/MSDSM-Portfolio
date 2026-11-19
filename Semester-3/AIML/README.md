# CrowdSight: Face Recognition in Crowded Area Video

**Team project**, Applied AI/ML, Semester 3, M.Sc. Data Science & Management. Built by Vivek Shivhare and Abhishek Dewangan.

## What it does

Detects and identifies people in crowded video, with a live dashboard.

**Pipeline:** frame, then face detection (SCRFD), 5-point alignment, ArcFace embedding, cosine matching against an enrolled gallery, ByteTrack-style tracking with voting, and a FastAPI dashboard.

## Notable design choices

- **Tiled detection for tiny faces.** Wide crowd shots hold very small faces. On a synthetic 2256×1560 test image with 384 tiny faces, normal detection found 108 faces in 0.19 s, and tiled detection found all 384 in 2.8 s on CPU. Normal mode suits live video, tiled mode suits wide shots.
- **Tracking with voting.** Identity is decided across a track's frames rather than from a single frame, which is steadier in crowds.
- **Ethics built in.** Only consenting people are enrolled in the gallery, and the dashboard includes a privacy mode that blurs unknown faces.

## Files

| Path | Description |
|------|-------------|
| `CrowdSight_Colab.ipynb` | The full pipeline and live dashboard, runnable in Google Colab |
| `crowd-face-recognition/` | Project folder for local use: source code, scripts, setup guide and results |

Setup steps for running locally are in the README inside `crowd-face-recognition/`.
