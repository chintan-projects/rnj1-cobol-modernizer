#!/bin/bash
#
# Essential AI Rnj-1 Demo - Quick Start
# =====================================
#
# This script verifies prerequisites and launches the interactive demo.
#

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

echo -e "${CYAN}"
echo "╔═══════════════════════════════════════════════════════════════════╗"
echo "║     Essential AI - Rnj-1 COBOL Modernization Demo                 ║"
echo "╚═══════════════════════════════════════════════════════════════════╝"
echo -e "${NC}"

# Check Ollama
echo -e "${YELLOW}Checking prerequisites...${NC}"

if ! command -v ollama &> /dev/null; then
    echo -e "${RED}✗ Ollama not found${NC}"
    echo "  Install: brew install ollama (macOS) or curl -fsSL https://ollama.com/install.sh | sh"
    exit 1
fi
echo -e "${GREEN}✓ Ollama installed${NC}"

# Check if Ollama is running
if ! curl -s http://localhost:11434/api/tags > /dev/null 2>&1; then
    echo -e "${YELLOW}Starting Ollama server...${NC}"
    ollama serve &
    sleep 3
fi
echo -e "${GREEN}✓ Ollama server running${NC}"

# Check for rnj-1 model
MODELS=$(ollama list 2>/dev/null || echo "")
if echo "$MODELS" | grep -qi "rnj"; then
    echo -e "${GREEN}✓ Rnj-1 model found${NC}"
    # Get exact model name
    MODEL_NAME=$(echo "$MODELS" | grep -i "rnj" | head -1 | awk '{print $1}')
    echo "  Using model: $MODEL_NAME"
else
    echo -e "${YELLOW}⚠ Rnj-1 model not found. Available models:${NC}"
    ollama list
    echo ""
    echo "To pull Rnj-1: ollama pull essentialai/rnj-v1-8b"
    echo "Or specify your model name when running the demo."
    MODEL_NAME="rnj-1"
fi

# Check Python
if ! command -v python3 &> /dev/null; then
    echo -e "${RED}✗ Python 3 not found${NC}"
    exit 1
fi
echo -e "${GREEN}✓ Python 3 available${NC}"

# Check requests library
if ! python3 -c "import requests" 2>/dev/null; then
    echo -e "${YELLOW}Installing requests library...${NC}"
    pip3 install requests
fi
echo -e "${GREEN}✓ Python dependencies ready${NC}"

echo ""
echo -e "${GREEN}All prerequisites satisfied!${NC}"
echo ""
echo -e "${CYAN}Starting interactive demo...${NC}"
echo -e "${YELLOW}Tip: Type /help for commands${NC}"
echo ""

# Get script directory
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Launch demo with default sample file
python3 "$SCRIPT_DIR/interactive_demo.py" "$SCRIPT_DIR/samples/CBACT04C.cbl"
