"""Day 1 check: are all libraries installed, and is a GPU available?

Run:  python scripts/check_env.py
"""
import sys

print(f"Python      : {sys.version.split()[0]}")
ok = True
for name in ["numpy", "cv2", "onnxruntime", "insightface"]:
    try:
        mod = __import__(name)
        print(f"{name:<12}: {getattr(mod, '__version__', 'ok')}")
    except Exception as e:  # noqa: BLE001
        ok = False
        print(f"{name:<12}: MISSING ({e})")

try:
    import onnxruntime as ort
    prov = ort.get_available_providers()
    print(f"Providers   : {prov}")
    print("Device      :", "GPU (CUDA) ✅" if "CUDAExecutionProvider" in prov else "CPU only")
except Exception:
    pass

print("\nAll good! ✅" if ok else "\nInstall missing packages: pip install -r requirements.txt")
