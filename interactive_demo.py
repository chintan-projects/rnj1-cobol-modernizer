#!/usr/bin/env python3
"""
Essential AI - Interactive COBOL Modernization Demo
====================================================

An interactive chat-style demo that helps bank CTOs understand Rnj-1's capabilities.
Shows Rnj-1's capabilities in a conversational format.

Usage:
    python interactive_demo.py [cobol_file]
    
Commands during session:
    /load <file>    - Load a COBOL file
    /analyze        - Analyze current file
    /document       - Generate documentation
    /python         - Convert to Python
    /java           - Convert to Java
    /test           - Generate test suite
    /compare        - Show COBOL vs converted side-by-side
    /metrics        - Show performance metrics
    /help           - Show commands
    /quit           - Exit demo

"""

import sys
import time
import json
import re
import os
from pathlib import Path
from typing import Optional, Tuple
import requests
import readline  # For better input handling

# =============================================================================
# Configuration
# =============================================================================

OLLAMA_BASE_URL = "http://localhost:11434"
MODEL_NAME = "rnj-1"

class Colors:
    HEADER = '\033[95m'
    BLUE = '\033[94m'
    CYAN = '\033[96m'
    GREEN = '\033[92m'
    YELLOW = '\033[93m'
    RED = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    DIM = '\033[2m'

# =============================================================================
# Demo Session
# =============================================================================

class InteractiveDemo:
    """Interactive demo session manager."""

    def __init__(self, output_dir: Optional[str] = None):
        self.current_file: Optional[str] = None
        self.current_code: Optional[str] = None
        self.output_dir = Path(output_dir) if output_dir else Path.cwd()
        self.session_metrics = {
            "queries": 0,
            "tokens_generated": 0,
            "total_time": 0,
            "operations": []
        }
        self.conversation_history = []

    def extract_code_blocks(self, response: str, language: str) -> list:
        """Extract code blocks for the specified language from model response."""
        pattern = rf'```{language}\s*\n(.*?)```'
        matches = re.findall(pattern, response, re.DOTALL | re.IGNORECASE)
        return [match.strip() for match in matches]

    def get_output_filename(self, target: str) -> str:
        """Generate output filename based on source COBOL file and target language."""
        if not self.current_file:
            return f"converted.{target[:2]}"
        base_name = Path(self.current_file).stem.lower()
        if target == "python":
            return f"{base_name}.py"
        elif target == "java":
            class_name = ''.join(word.capitalize() for word in base_name.replace('_', ' ').replace('-', ' ').split())
            return f"{class_name}.java"
        return base_name

    def save_converted_code(self, target: str, response: str) -> Tuple[Path, list]:
        """Save converted code to appropriate directory."""
        target_dir = self.output_dir / target
        target_dir.mkdir(parents=True, exist_ok=True)

        code_blocks = self.extract_code_blocks(response, target)
        saved_files = []
        source_name = Path(self.current_file).stem if self.current_file else "converted"

        if code_blocks:
            main_filename = self.get_output_filename(target)
            main_filepath = target_dir / main_filename
            with open(main_filepath, 'w', encoding='utf-8') as f:
                f.write(code_blocks[0])
            saved_files.append(main_filepath)

            for i, code in enumerate(code_blocks[1:], 2):
                if target == "python":
                    extra_filename = f"{source_name.lower()}_part{i}.py"
                else:
                    extra_filename = f"{source_name.replace('-', '').replace('_', '')}Part{i}.java"
                extra_filepath = target_dir / extra_filename
                with open(extra_filepath, 'w', encoding='utf-8') as f:
                    f.write(code)
                saved_files.append(extra_filepath)

        readme_path = target_dir / "README.md"
        readme_content = self.generate_readme(target, saved_files)
        with open(readme_path, 'w', encoding='utf-8') as f:
            f.write(readme_content)
        saved_files.append(readme_path)

        return target_dir, saved_files

    def generate_readme(self, target: str, saved_files: list) -> str:
        """Generate README with execution instructions."""
        source_name = Path(self.current_file).stem if self.current_file else "converted"
        main_file = saved_files[0].name if saved_files else "converted_code"

        if target == "python":
            return f"""# {source_name} - Python Conversion

## Source
Converted from COBOL file: `{Path(self.current_file).name if self.current_file else 'unknown'}`

## Files
{chr(10).join(f"- `{f.name}`" for f in saved_files if f.suffix == '.py')}

## Execution Instructions

### Prerequisites
```bash
pip install dataclasses typing
```

### Run the Program
```bash
python {main_file}
```

### Run Tests (if included)
```bash
pytest {main_file} -v
```

---
*Generated by Essential AI Rnj-1*
"""
        else:
            class_name = Path(main_file).stem
            return f"""# {source_name} - Java Conversion

## Source
Converted from COBOL file: `{Path(self.current_file).name if self.current_file else 'unknown'}`

## Files
{chr(10).join(f"- `{f.name}`" for f in saved_files if f.suffix == '.java')}

## Execution Instructions

### Compile
```bash
javac {main_file}
```

### Run
```bash
java {class_name}
```

---
*Generated by Essential AI Rnj-1*
"""
    
    def banner(self):
        """Display the demo banner."""
        print(f"""
{Colors.BOLD}{Colors.CYAN}
    ╔═══════════════════════════════════════════════════════════════════════════╗
    ║                                                                           ║
    ║     ███████╗███████╗███████╗███████╗███╗   ██╗████████╗██╗ █████╗ ██╗     ║
    ║     ██╔════╝██╔════╝██╔════╝██╔════╝████╗  ██║╚══██╔══╝██║██╔══██╗██║     ║
    ║     █████╗  ███████╗███████╗█████╗  ██╔██╗ ██║   ██║   ██║███████║██║     ║
    ║     ██╔══╝  ╚════██║╚════██║██╔══╝  ██║╚██╗██║   ██║   ██║██╔══██║██║     ║
    ║     ███████╗███████║███████║███████╗██║ ╚████║   ██║   ██║██║  ██║███████╗║
    ║     ╚══════╝╚══════╝╚══════╝╚══════╝╚═╝  ╚═══╝   ╚═╝   ╚═╝╚═╝  ╚═╝╚══════╝║
    ║                                                                           ║
    ║              Rnj-1: Sovereign AI for COBOL Modernization                  ║
    ║                                                                           ║
    ╚═══════════════════════════════════════════════════════════════════════════╝
{Colors.ENDC}
{Colors.BOLD}    Key Differentiators:{Colors.ENDC}
    {Colors.GREEN}✓{Colors.ENDC} 100% Local Inference - Your code never leaves your network
    {Colors.GREEN}✓{Colors.ENDC} 8B Parameters - Runs on a single GPU or high-end laptop  
    {Colors.GREEN}✓{Colors.ENDC} Apache 2.0 License - No usage restrictions or per-token fees
    {Colors.GREEN}✓{Colors.ENDC} Program Execution Modeling - Understands what code DOES, not just syntax

{Colors.DIM}    Type /help for commands or just ask questions about your COBOL code{Colors.ENDC}
""")
    
    def check_ollama(self) -> bool:
        """Verify Ollama is running."""
        try:
            response = requests.get(f"{OLLAMA_BASE_URL}/api/tags", timeout=5)
            if response.status_code == 200:
                models = [m.get("name", "") for m in response.json().get("models", [])]
                if any(MODEL_NAME in m for m in models):
                    print(f"{Colors.GREEN}✓ Connected to Ollama with {MODEL_NAME}{Colors.ENDC}")
                    return True
                print(f"{Colors.RED}✗ Model {MODEL_NAME} not found{Colors.ENDC}")
                print(f"  Available models: {', '.join(models)}")
                return False
            return False
        except:
            print(f"{Colors.RED}✗ Cannot connect to Ollama at {OLLAMA_BASE_URL}{Colors.ENDC}")
            return False
    
    def load_file(self, filepath: str) -> bool:
        """Load a COBOL file."""
        path = Path(filepath)
        if not path.exists():
            print(f"{Colors.RED}File not found: {filepath}{Colors.ENDC}")
            return False
        
        with open(path, 'r', encoding='utf-8', errors='replace') as f:
            self.current_code = f.read()
        
        self.current_file = filepath
        lines = len(self.current_code.splitlines())
        chars = len(self.current_code)
        
        print(f"{Colors.GREEN}✓ Loaded: {filepath}{Colors.ENDC}")
        print(f"  {lines} lines, {chars} characters")
        
        # Show first few lines as preview
        preview_lines = self.current_code.splitlines()[:10]
        print(f"\n{Colors.DIM}Preview:{Colors.ENDC}")
        for i, line in enumerate(preview_lines, 1):
            print(f"{Colors.DIM}{i:4}: {line[:70]}{Colors.ENDC}")
        if lines > 10:
            print(f"{Colors.DIM}  ... ({lines - 10} more lines){Colors.ENDC}")
        
        return True
    
    def query_model(self, prompt: str, system: Optional[str] = None) -> str:
        """Send query to Rnj-1 and stream response."""
        start_time = time.time()
        
        payload = {
            "model": MODEL_NAME,
            "prompt": prompt,
            "stream": True,
            "options": {
                "temperature": 0.1,
                "num_predict": 4096,
            }
        }
        
        if system:
            payload["system"] = system
        
        try:
            response = requests.post(
                f"{OLLAMA_BASE_URL}/api/generate",
                json=payload,
                stream=True,
                timeout=300
            )
            response.raise_for_status()
            
            full_response = []
            print()  # Start on new line
            
            for line in response.iter_lines():
                if line:
                    data = json.loads(line)
                    if "response" in data:
                        chunk = data["response"]
                        print(chunk, end="", flush=True)
                        full_response.append(chunk)
                    if data.get("done", False):
                        break
            
            print()  # End with newline
            
            elapsed = time.time() - start_time
            result = "".join(full_response)
            
            # Update metrics
            self.session_metrics["queries"] += 1
            self.session_metrics["total_time"] += elapsed
            self.session_metrics["tokens_generated"] += len(result.split())
            self.session_metrics["operations"].append({
                "time": elapsed,
                "tokens": len(result.split())
            })
            
            print(f"\n{Colors.DIM}[{elapsed:.1f}s, ~{len(result.split())} tokens]{Colors.ENDC}")
            
            return result
            
        except Exception as e:
            print(f"{Colors.RED}Error: {e}{Colors.ENDC}")
            return ""
    
    def cmd_analyze(self):
        """Analyze current COBOL file."""
        if not self.current_code:
            print(f"{Colors.YELLOW}No file loaded. Use /load <file> first.{Colors.ENDC}")
            return
        
        print(f"{Colors.CYAN}Analyzing {self.current_file}...{Colors.ENDC}")
        
        prompt = f"""Analyze this COBOL program and provide:
1. Program purpose and type (batch/CICS)
2. Key data structures
3. Main processing flow
4. External dependencies
5. Modernization complexity rating (1-10)

```cobol
{self.current_code}
```"""
        
        self.query_model(prompt, system="You are an expert COBOL analyst. Be concise and precise.")
    
    def cmd_document(self):
        """Generate documentation for current file."""
        if not self.current_code:
            print(f"{Colors.YELLOW}No file loaded. Use /load <file> first.{Colors.ENDC}")
            return
        
        print(f"{Colors.CYAN}Generating documentation...{Colors.ENDC}")
        
        prompt = f"""Generate technical documentation for this COBOL program including:
- Executive summary (2 sentences)
- Functional specification
- Data dictionary (key fields only)
- Process flow

```cobol
{self.current_code}
```"""
        
        self.query_model(prompt, system="You are a technical writer specializing in mainframe documentation.")
    
    def cmd_convert_python(self):
        """Convert to Python."""
        if not self.current_code:
            print(f"{Colors.YELLOW}No file loaded. Use /load <file> first.{Colors.ENDC}")
            return

        print(f"{Colors.CYAN}Converting to Python...{Colors.ENDC}")

        prompt = f"""Convert this COBOL program to modern Python 3.10+:
- Use dataclasses and type hints
- Preserve all business logic
- Include docstrings
- Make it production-ready

```cobol
{self.current_code}
```

Output the Python code with brief conversion notes."""

        result = self.query_model(prompt, system="You are an expert at COBOL to Python migration. Write clean, idiomatic Python.")

        # Save converted code
        if result:
            target_dir, saved_files = self.save_converted_code("python", result)
            print(f"\n{Colors.GREEN}✓ Saved to: {target_dir}/{Colors.ENDC}")
            for f in saved_files:
                print(f"  {Colors.CYAN}• {f.name}{Colors.ENDC}")
    
    def cmd_convert_java(self):
        """Convert to Java."""
        if not self.current_code:
            print(f"{Colors.YELLOW}No file loaded. Use /load <file> first.{Colors.ENDC}")
            return

        print(f"{Colors.CYAN}Converting to Java...{Colors.ENDC}")

        prompt = f"""Convert this COBOL program to Java 17+:
- Use records for data structures
- Follow enterprise patterns
- Include Javadoc
- Make it Spring Boot compatible

```cobol
{self.current_code}
```

Output the Java code with brief conversion notes."""

        result = self.query_model(prompt, system="You are an expert at COBOL to Java migration. Write clean, enterprise Java.")

        # Save converted code
        if result:
            target_dir, saved_files = self.save_converted_code("java", result)
            print(f"\n{Colors.GREEN}✓ Saved to: {target_dir}/{Colors.ENDC}")
            for f in saved_files:
                print(f"  {Colors.CYAN}• {f.name}{Colors.ENDC}")
    
    def cmd_test(self):
        """Generate test cases."""
        if not self.current_code:
            print(f"{Colors.YELLOW}No file loaded. Use /load <file> first.{Colors.ENDC}")
            return
        
        print(f"{Colors.CYAN}Generating test cases...{Colors.ENDC}")
        
        prompt = f"""Generate a test suite for this COBOL program:
- Key positive test cases
- Edge cases and boundary conditions
- Error handling tests

```cobol
{self.current_code}
```

Format as a test case table with ID, description, input, expected output."""
        
        self.query_model(prompt, system="You are a QA engineer specializing in mainframe testing.")
    
    def cmd_metrics(self):
        """Show session metrics."""
        print(f"\n{Colors.CYAN}Session Metrics{Colors.ENDC}")
        print(f"{'─' * 40}")
        print(f"  Queries:          {self.session_metrics['queries']}")
        print(f"  Tokens generated: ~{self.session_metrics['tokens_generated']}")
        print(f"  Total time:       {self.session_metrics['total_time']:.1f}s")
        if self.session_metrics['queries'] > 0:
            avg_time = self.session_metrics['total_time'] / self.session_metrics['queries']
            print(f"  Avg per query:    {avg_time:.1f}s")
        print(f"\n{Colors.GREEN}Note: All processing done locally - zero API costs!{Colors.ENDC}")
    
    def cmd_help(self):
        """Show help."""
        print(f"""
{Colors.BOLD}Commands:{Colors.ENDC}
  {Colors.CYAN}/load <file>{Colors.ENDC}  - Load a COBOL file
  {Colors.CYAN}/analyze{Colors.ENDC}      - Analyze current file
  {Colors.CYAN}/document{Colors.ENDC}     - Generate documentation
  {Colors.CYAN}/python{Colors.ENDC}       - Convert to Python
  {Colors.CYAN}/java{Colors.ENDC}         - Convert to Java
  {Colors.CYAN}/test{Colors.ENDC}         - Generate test suite
  {Colors.CYAN}/metrics{Colors.ENDC}      - Show performance metrics
  {Colors.CYAN}/help{Colors.ENDC}         - Show this help
  {Colors.CYAN}/quit{Colors.ENDC}         - Exit demo

{Colors.BOLD}Or just type a question:{Colors.ENDC}
  "What does the PERFORM loop in line 150 do?"
  "Explain the error handling in this program"
  "What database operations does this program perform?"
""")
    
    def handle_free_query(self, query: str):
        """Handle free-form questions."""
        if not self.current_code:
            print(f"{Colors.YELLOW}No file loaded. Use /load <file> first.{Colors.ENDC}")
            return
        
        prompt = f"""Given this COBOL program:

```cobol
{self.current_code}
```

Answer this question: {query}"""
        
        self.query_model(prompt, system="You are an expert COBOL programmer. Answer precisely and concisely.")
    
    def run(self, initial_file: Optional[str] = None):
        """Run the interactive demo."""
        self.banner()
        
        if not self.check_ollama():
            print(f"\n{Colors.YELLOW}Please start Ollama and ensure {MODEL_NAME} is available.{Colors.ENDC}")
            sys.exit(1)
        
        if initial_file:
            self.load_file(initial_file)
        
        print(f"\n{Colors.GREEN}Ready! Type /help for commands or ask questions.{Colors.ENDC}\n")
        
        while True:
            try:
                # Prompt with context
                if self.current_file:
                    prompt_text = f"{Colors.BOLD}[{Path(self.current_file).name}]{Colors.ENDC} > "
                else:
                    prompt_text = f"{Colors.BOLD}rnj-1{Colors.ENDC} > "
                
                user_input = input(prompt_text).strip()
                
                if not user_input:
                    continue
                
                # Handle commands
                if user_input.startswith('/'):
                    parts = user_input.split(maxsplit=1)
                    cmd = parts[0].lower()
                    arg = parts[1] if len(parts) > 1 else None
                    
                    if cmd == '/quit' or cmd == '/exit' or cmd == '/q':
                        print(f"\n{Colors.CYAN}Thanks for the demo! Metrics:{Colors.ENDC}")
                        self.cmd_metrics()
                        break
                    elif cmd == '/load' and arg:
                        self.load_file(arg)
                    elif cmd == '/analyze':
                        self.cmd_analyze()
                    elif cmd == '/document':
                        self.cmd_document()
                    elif cmd == '/python':
                        self.cmd_convert_python()
                    elif cmd == '/java':
                        self.cmd_convert_java()
                    elif cmd == '/test':
                        self.cmd_test()
                    elif cmd == '/metrics':
                        self.cmd_metrics()
                    elif cmd == '/help':
                        self.cmd_help()
                    else:
                        print(f"{Colors.YELLOW}Unknown command. Type /help for available commands.{Colors.ENDC}")
                else:
                    # Free-form question
                    self.handle_free_query(user_input)
                    
            except KeyboardInterrupt:
                print(f"\n{Colors.YELLOW}Use /quit to exit{Colors.ENDC}")
            except EOFError:
                break
        
        print(f"\n{Colors.CYAN}Goodbye!{Colors.ENDC}")

# =============================================================================
# Main
# =============================================================================

if __name__ == "__main__":
    demo = InteractiveDemo()
    initial_file = sys.argv[1] if len(sys.argv) > 1 else None
    demo.run(initial_file)
