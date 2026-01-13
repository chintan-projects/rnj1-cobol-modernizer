# Essential AI Rnj-1 COBOL Modernization Demo

## Sovereign AI for Enterprise Legacy Modernization

This demo toolkit showcases Essential AI's Rnj-1 model for COBOL code analysis, documentation, and modernization—running **100% locally** with no cloud dependencies.

---

## 🎯 Key Value Propositions for Bank CTOs

| Concern | Rnj-1 Solution |
|---------|----------------|
| **Data Sovereignty** | Code never leaves your network—all inference runs locally |
| **Compliance** | No third-party API calls = no data residency concerns |
| **Cost** | Zero per-token costs; unlimited usage after hardware investment |
| **Air-Gap Ready** | Works in fully disconnected environments |
| **Customization** | Apache 2.0 license allows fine-tuning on your codebase |

---

## 📋 Prerequisites

### 1. Install Ollama
```bash
# macOS
brew install ollama

# Linux
curl -fsSL https://ollama.com/install.sh | sh
```

### 2. Pull Rnj-1 Model
```bash
ollama pull essentialai/rnj-v1-8b
# Or if you have it locally as 'rnj-1':
ollama list  # Verify model name
```

### 3. Start Ollama Server
```bash
ollama serve
# Leave this running in a terminal
```

### 4. Install Python Dependencies
```bash
pip install requests
```

---

## 🚀 Quick Start

### Interactive Demo (Recommended for Live Presentations)

```bash
cd rnj1-cobol-demo
python interactive_demo.py samples/CBACT04C.cbl
```

Then use commands like:
- `/analyze` - Analyze the COBOL program
- `/python` - Convert to Python
- `/java` - Convert to Java
- `/test` - Generate test cases
- Or ask any question: "What business rules are in this program?"

### Command-Line Tool

```bash
# Analyze a COBOL file
python cobol_modernizer.py analyze samples/CBACT04C.cbl

# Generate documentation
python cobol_modernizer.py document samples/CBACT04C.cbl

# Convert to Python
python cobol_modernizer.py convert samples/CBACT04C.cbl --target python

# Convert to Java  
python cobol_modernizer.py convert samples/CBACT04C.cbl --target java

# Generate test suite
python cobol_modernizer.py test samples/CBACT04C.cbl

# Full pipeline (all steps)
python cobol_modernizer.py full samples/CBACT04C.cbl --target python
```

---

## 📁 Sample Files

| File | Type | Description |
|------|------|-------------|
| `CBACT04C.cbl` | Batch | Credit card interest calculator |
| `COACTVWC.cbl` | CICS | Online account inquiry transaction |

These are based on AWS's [CardDemo](https://github.com/aws-samples/aws-mainframe-modernization-carddemo) application—a realistic credit card management system used for mainframe modernization testing.

---




## 🔧 Configuration

### Using a Different Model Name

If your model has a different name in Ollama:

```bash
# Check available models
ollama list

# Use with custom model name
python cobol_modernizer.py analyze samples/CBACT04C.cbl --model your-model-name
```

### Environment Variables

```bash
export OLLAMA_BASE_URL=http://localhost:11434  # Default
export OLLAMA_MODEL=rnj-1                       # Your model name
```

---

## 📊 Performance Benchmarks

Typical performance on M1 MacBook Pro (16GB):

| Operation | Time | Tokens |
|-----------|------|--------|
| Analyze | 15-30s | ~500 |
| Document | 20-40s | ~800 |
| Convert to Python | 30-60s | ~1000 |
| Generate Tests | 20-40s | ~600 |
| Full Pipeline | 2-3min | ~3000 |

---

## 🏦 Using with Real COBOL

To use with your organization's COBOL:

```bash
# Single file
python cobol_modernizer.py analyze /path/to/YOUR-PROGRAM.cbl

# Interactive exploration
python interactive_demo.py /path/to/YOUR-PROGRAM.cbl
```

For batch processing multiple files, use the CLI tool with shell scripting:

```bash
for f in /path/to/cobol/*.cbl; do
    python cobol_modernizer.py document "$f" -o "docs/$(basename $f .cbl).md"
done
```

---

## ⚠️ Limitations & Considerations

1. **Context Window**: Rnj-1 has 32K token context. Very large COBOL programs (>5000 lines) may need to be split.

2. **Copybooks**: The demo doesn't automatically resolve COPY statements. For full analysis, inline the copybooks first.

3. **DB2/CICS**: Complex DB2 SQL and CICS commands are recognized but may need manual review for conversion accuracy.

4. **Testing Required**: All generated code should go through your standard QA process before production use.

---


This demo toolkit: Apache 2.0
Rnj-1 Model: Apache 2.0  
Sample COBOL (CardDemo): Apache 2.0

---
