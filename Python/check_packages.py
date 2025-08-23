import importlib
import subprocess
import sys

# List of required PyPI packages
required_packages = [
    "pandas",
    "numpy",
    "scikit-learn",
    "joblib"
]

def install_with_pip(package):
    subprocess.check_call([sys.executable, "-m", "pip", "install", package])

# Check and install required packages
for package in required_packages:
    try:
        importlib.import_module(package)
        print(f"{package}: OK")
    except ImportError:
        print(f"{package} not found. Installing via pip...")
        install_with_pip(package)

# Check for local or custom modules
custom_modules = ["lccml", "evaluation", "confusion_matrix", "measures"]

for module in custom_modules:
    try:
        importlib.import_module(module)
        print(f"{module}: OK")
    except ImportError:
        print(f"❌ {module} not found. Make sure the module is in the correct directory or in PYTHONPATH.")
