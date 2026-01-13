#!/usr/bin/env python3
"""
Essential AI Rnj-1 COBOL Modernization Demo
============================================

A standalone demonstration tool showcasing Rnj-1's capabilities for
enterprise COBOL code analysis, documentation, and modernization.

Target Audience: Bank CTOs evaluating sovereign AI solutions
Use Case: Legacy mainframe modernization without cloud dependencies

Requirements:
    - Ollama running locally with rnj-1 model
    - Python 3.9+
    - requests library

Usage:
    python cobol_modernizer.py analyze <cobol_file>
    python cobol_modernizer.py document <cobol_file>
    python cobol_modernizer.py convert <cobol_file> --target python
    python cobol_modernizer.py convert <cobol_file> --target java
    python cobol_modernizer.py test <cobol_file>
    python cobol_modernizer.py full <cobol_file>  # Complete pipeline

License: Apache 2.0
"""

import argparse
import json
import os
import re
import sys
import time
from pathlib import Path
from typing import Optional, Generator, Tuple
import requests

# =============================================================================
# Configuration
# =============================================================================

OLLAMA_BASE_URL = "http://localhost:11434"
MODEL_NAME = "rnj-1"
DEFAULT_TIMEOUT = 300  # 5 minutes for complex operations

# Styling for terminal output
class Colors:
    HEADER = '\033[95m'
    BLUE = '\033[94m'
    CYAN = '\033[96m'
    GREEN = '\033[92m'
    YELLOW = '\033[93m'
    RED = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'

# =============================================================================
# Ollama API Client
# =============================================================================

class OllamaClient:
    """Client for interacting with Ollama API."""
    
    def __init__(self, base_url: str = OLLAMA_BASE_URL, model: str = MODEL_NAME):
        self.base_url = base_url
        self.model = model
        self.session = requests.Session()
    
    def check_health(self) -> bool:
        """Verify Ollama is running and model is available."""
        try:
            response = self.session.get(f"{self.base_url}/api/tags", timeout=5)
            if response.status_code == 200:
                models = response.json().get("models", [])
                model_names = [m.get("name", "").split(":")[0] for m in models]
                return self.model.split(":")[0] in model_names or any(self.model in m.get("name", "") for m in models)
            return False
        except requests.exceptions.RequestException:
            return False
    
    def generate(self, prompt: str, system: Optional[str] = None, 
                 stream: bool = True, temperature: float = 0.1) -> Generator[str, None, None]:
        """Generate completion from Rnj-1."""
        payload = {
            "model": self.model,
            "prompt": prompt,
            "stream": stream,
            "options": {
                "temperature": temperature,
                "num_predict": 4096,
            }
        }
        
        if system:
            payload["system"] = system
        
        try:
            response = self.session.post(
                f"{self.base_url}/api/generate",
                json=payload,
                stream=stream,
                timeout=DEFAULT_TIMEOUT
            )
            response.raise_for_status()
            
            if stream:
                for line in response.iter_lines():
                    if line:
                        data = json.loads(line)
                        if "response" in data:
                            yield data["response"]
                        if data.get("done", False):
                            break
            else:
                data = response.json()
                yield data.get("response", "")
                
        except requests.exceptions.RequestException as e:
            raise ConnectionError(f"Failed to connect to Ollama: {e}")

# =============================================================================
# COBOL Analysis Prompts
# =============================================================================

SYSTEM_PROMPT = """You are an expert mainframe systems analyst with 30+ years of experience in COBOL, CICS, and enterprise banking systems. You have deep expertise in:

- COBOL-85 and COBOL-2002 standards
- CICS transaction processing
- DB2 and VSAM file handling
- JCL and batch processing
- Banking and financial services domain knowledge
- Code modernization and migration strategies

You provide precise, technically accurate analysis suitable for senior engineering leadership and CTOs. Your outputs are structured, actionable, and demonstrate deep understanding of both legacy constraints and modern alternatives."""

ANALYZE_PROMPT = """Analyze this COBOL program and provide a comprehensive technical assessment:

```cobol
{code}
```

Provide your analysis in the following structure:

## Program Overview
- Program name and type (batch/CICS/subroutine)
- Primary business function
- Estimated lines of code and complexity rating (1-10)

## Data Structures
- Key working storage variables
- File definitions and record layouts
- Copybooks referenced

## Business Logic Flow
- Main processing steps (numbered)
- Decision points and branching logic
- Error handling approach

## External Dependencies
- Called programs/subroutines
- Database operations (DB2/VSAM)
- CICS commands used
- External files accessed

## Modernization Assessment
- Complexity factors for migration
- Recommended modernization approach
- Estimated effort (person-days)
- Risk factors to consider

## Key Business Rules
List the critical business rules embedded in this code that must be preserved during modernization."""

DOCUMENT_PROMPT = """Generate comprehensive technical documentation for this COBOL program:

```cobol
{code}
```

Create documentation suitable for:
1. New developers onboarding to the system
2. Business analysts understanding the logic
3. QA teams creating test cases
4. Architects planning modernization

Include:

## Executive Summary
(2-3 sentences for leadership)

## Functional Specification
- Business purpose
- Input/output specifications
- Processing rules

## Technical Specification
- Program structure
- Data flow diagram (text-based)
- Interface contracts

## Data Dictionary
| Field Name | Type | Length | Description | Business Rule |
|------------|------|--------|-------------|---------------|
(List all significant data fields)

## Process Flow
```
[Text-based flowchart showing main logic]
```

## Error Handling
- Error conditions detected
- Error responses/codes
- Recovery procedures

## Testing Considerations
- Suggested test scenarios
- Edge cases to cover
- Data dependencies"""

CONVERT_PYTHON_PROMPT = """Convert this COBOL program to modern Python with full type hints:

```cobol
{code}
```

Requirements:
1. Use Python 3.10+ with dataclasses and type hints
2. Preserve ALL business logic exactly
3. Use appropriate Python idioms (not literal translation)
4. Include comprehensive docstrings
5. Add logging for auditability
6. Handle errors with proper exceptions
7. Make the code production-ready

Structure your output as:

## Conversion Notes
(Key decisions made during conversion)

## Python Code
```python
# Full implementation here
```

## Data Models
```python
# Dataclass definitions for record structures
```

## Unit Tests
```python
# pytest test cases covering key business rules
```

## Migration Checklist
- [ ] Items to verify during testing"""

CONVERT_JAVA_PROMPT = """Convert this COBOL program to modern Java (17+):

```cobol
{code}
```

Requirements:
1. Use Java 17+ features (records, sealed classes where appropriate)
2. Follow enterprise Java patterns (dependency injection ready)
3. Preserve ALL business logic exactly
4. Use appropriate Java idioms (not literal translation)
5. Include Javadoc documentation
6. Add SLF4J logging for auditability
7. Use Optional for null safety
8. Make the code Spring Boot compatible

Structure your output as:

## Conversion Notes
(Key decisions made during conversion)

## Java Implementation
```java
// Main class implementation
```

## Data Transfer Objects
```java
// Record/class definitions for data structures
```

## Unit Tests
```java
// JUnit 5 test cases
```

## Spring Configuration
```java
// Bean configuration if applicable
```

## Migration Checklist
- [ ] Items to verify during testing"""

TEST_GENERATION_PROMPT = """Generate a comprehensive test suite for this COBOL program:

```cobol
{code}
```

Create tests that:
1. Cover all business logic paths
2. Test boundary conditions
3. Verify error handling
4. Can be used for regression testing after modernization

Output:

## Test Strategy
- Testing approach
- Coverage goals
- Data requirements

## Test Cases

### Positive Tests
| ID | Description | Input | Expected Output | Business Rule Verified |
|----|-------------|-------|-----------------|----------------------|

### Negative Tests
| ID | Description | Input | Expected Error | Error Handling Verified |
|----|-------------|-------|----------------|------------------------|

### Boundary Tests
| ID | Description | Boundary Condition | Expected Behavior |
|----|-------------|-------------------|-------------------|

## Test Data Requirements
- Sample data needed
- Data generation approach

## Automation Script
```python
# pytest-compatible test structure
```"""

# =============================================================================
# Demo Runner
# =============================================================================

class COBOLModernizer:
    """Main demo class for COBOL modernization with Rnj-1."""

    def __init__(self, output_dir: Optional[str] = None):
        self.client = OllamaClient()
        self.start_time = None
        self.output_dir = Path(output_dir) if output_dir else Path.cwd()

    def extract_code_blocks(self, response: str, language: str) -> list:
        """Extract code blocks for the specified language from model response."""
        pattern = rf'```{language}\s*\n(.*?)```'
        matches = re.findall(pattern, response, re.DOTALL | re.IGNORECASE)
        return [match.strip() for match in matches]

    def get_output_filename(self, source_file: str, target: str) -> str:
        """Generate output filename based on source COBOL file and target language."""
        source_path = Path(source_file)
        base_name = source_path.stem.lower()

        if target == "python":
            return f"{base_name}.py"
        elif target == "java":
            # Java class names should be PascalCase
            class_name = ''.join(word.capitalize() for word in base_name.replace('_', ' ').replace('-', ' ').split())
            return f"{class_name}.java"
        return base_name

    def save_converted_code(self, source_file: str, target: str, response: str) -> Tuple[Path, list]:
        """Save converted code to appropriate directory with executable instructions."""
        # Create target directory
        target_dir = self.output_dir / target
        target_dir.mkdir(parents=True, exist_ok=True)

        # Extract code blocks
        code_blocks = self.extract_code_blocks(response, target)

        saved_files = []
        source_name = Path(source_file).stem

        if code_blocks:
            # Save main code file
            main_filename = self.get_output_filename(source_file, target)
            main_filepath = target_dir / main_filename

            # Combine all code blocks (main code usually first)
            main_code = code_blocks[0]
            with open(main_filepath, 'w', encoding='utf-8') as f:
                f.write(main_code)
            saved_files.append(main_filepath)

            # If there are additional code blocks (tests, DTOs, etc.), save them separately
            if len(code_blocks) > 1:
                for i, code in enumerate(code_blocks[1:], 2):
                    if target == "python":
                        extra_filename = f"{source_name.lower()}_part{i}.py"
                    else:
                        extra_filename = f"{source_name.replace('-', '').replace('_', '')}Part{i}.java"
                    extra_filepath = target_dir / extra_filename
                    with open(extra_filepath, 'w', encoding='utf-8') as f:
                        f.write(code)
                    saved_files.append(extra_filepath)

        # Generate README with execution instructions
        readme_path = target_dir / "README.md"
        readme_content = self.generate_readme(source_file, target, saved_files)
        with open(readme_path, 'w', encoding='utf-8') as f:
            f.write(readme_content)
        saved_files.append(readme_path)

        return target_dir, saved_files

    def generate_readme(self, source_file: str, target: str, saved_files: list) -> str:
        """Generate README with execution instructions for the converted code."""
        source_name = Path(source_file).stem
        main_file = saved_files[0].name if saved_files else "converted_code"

        if target == "python":
            return f"""# {source_name} - Python Conversion

## Source
Converted from COBOL file: `{Path(source_file).name}`

## Files
{chr(10).join(f"- `{f.name}`" for f in saved_files if f.suffix == '.py')}

## Prerequisites
- Python 3.10 or higher
- Required dependencies (install via pip):
  ```bash
  pip install dataclasses typing
  ```

## Execution Instructions

### Running the Program
```bash
cd {target}
python {main_file}
```

### Running Tests (if included)
```bash
cd {target}
pytest {main_file} -v
```

### Import as Module
```python
from {Path(main_file).stem} import *
```

## Notes
- This code was auto-generated from COBOL using Essential AI's Rnj-1 model
- Review and test thoroughly before production use
- All business logic from the original COBOL has been preserved

---
*Generated by Essential AI Rnj-1 COBOL Modernization Demo*
"""
        else:  # Java
            class_name = Path(main_file).stem
            return f"""# {source_name} - Java Conversion

## Source
Converted from COBOL file: `{Path(source_file).name}`

## Files
{chr(10).join(f"- `{f.name}`" for f in saved_files if f.suffix == '.java')}

## Prerequisites
- Java 17 or higher
- Maven or Gradle (optional, for dependency management)

## Execution Instructions

### Compile the Code
```bash
cd {target}
javac {main_file}
```

### Run the Program
```bash
cd {target}
java {class_name}
```

### Using with Maven
Add to your `pom.xml`:
```xml
<dependencies>
    <dependency>
        <groupId>org.slf4j</groupId>
        <artifactId>slf4j-api</artifactId>
        <version>2.0.9</version>
    </dependency>
</dependencies>
```

### Using with Spring Boot
The generated code is designed to be Spring Boot compatible. Add as a component:
```java
@Component
public class {class_name}Service {{
    // Inject the converted class
}}
```

## Notes
- This code was auto-generated from COBOL using Essential AI's Rnj-1 model
- Review and test thoroughly before production use
- All business logic from the original COBOL has been preserved

---
*Generated by Essential AI Rnj-1 COBOL Modernization Demo*
"""
    
    def print_header(self, title: str):
        """Print a formatted header."""
        print(f"\n{Colors.BOLD}{Colors.CYAN}{'='*70}{Colors.ENDC}")
        print(f"{Colors.BOLD}{Colors.CYAN}  {title}{Colors.ENDC}")
        print(f"{Colors.BOLD}{Colors.CYAN}{'='*70}{Colors.ENDC}\n")
    
    def print_subheader(self, title: str):
        """Print a formatted subheader."""
        print(f"\n{Colors.YELLOW}>>> {title}{Colors.ENDC}\n")
    
    def print_status(self, message: str, status: str = "info"):
        """Print a status message."""
        colors = {
            "info": Colors.BLUE,
            "success": Colors.GREEN,
            "warning": Colors.YELLOW,
            "error": Colors.RED
        }
        color = colors.get(status, Colors.BLUE)
        symbols = {"info": "ℹ", "success": "✓", "warning": "⚠", "error": "✗"}
        symbol = symbols.get(status, "•")
        print(f"{color}{symbol} {message}{Colors.ENDC}")
    
    def stream_output(self, generator: Generator[str, None, None]):
        """Stream output with visual feedback."""
        full_response = []
        for chunk in generator:
            print(chunk, end="", flush=True)
            full_response.append(chunk)
        print()  # Final newline
        return "".join(full_response)
    
    def read_cobol_file(self, filepath: str) -> str:
        """Read COBOL source file."""
        path = Path(filepath)
        if not path.exists():
            raise FileNotFoundError(f"COBOL file not found: {filepath}")
        
        # Handle common COBOL file extensions
        if path.suffix.lower() not in ['.cbl', '.cob', '.cobol', '.cpy', '']:
            self.print_status(f"Warning: Unusual file extension {path.suffix}", "warning")
        
        with open(path, 'r', encoding='utf-8', errors='replace') as f:
            return f.read()
    
    def check_prerequisites(self) -> bool:
        """Check that Ollama and Rnj-1 are available."""
        self.print_status("Checking Ollama connection...", "info")
        
        if not self.client.check_health():
            self.print_status("Ollama not running or rnj-1 model not found!", "error")
            print(f"\n{Colors.YELLOW}To fix this:{Colors.ENDC}")
            print("  1. Start Ollama: ollama serve")
            print("  2. Pull the model: ollama pull essentialai/rnj-v1-8b")
            print("     Or if you have it locally: ollama list")
            return False
        
        self.print_status(f"Connected to Ollama with model: {MODEL_NAME}", "success")
        return True
    
    def analyze(self, filepath: str) -> str:
        """Analyze COBOL program structure and logic."""
        self.print_header("COBOL Program Analysis")
        
        code = self.read_cobol_file(filepath)
        self.print_status(f"Loaded: {filepath} ({len(code)} characters)", "info")
        
        prompt = ANALYZE_PROMPT.format(code=code)
        
        self.print_subheader("Analysis Results")
        self.start_time = time.time()
        
        result = self.stream_output(self.client.generate(prompt, system=SYSTEM_PROMPT))
        
        elapsed = time.time() - self.start_time
        self.print_status(f"Analysis completed in {elapsed:.1f}s", "success")
        
        return result
    
    def document(self, filepath: str) -> str:
        """Generate comprehensive documentation."""
        self.print_header("Documentation Generation")
        
        code = self.read_cobol_file(filepath)
        self.print_status(f"Loaded: {filepath} ({len(code)} characters)", "info")
        
        prompt = DOCUMENT_PROMPT.format(code=code)
        
        self.print_subheader("Generated Documentation")
        self.start_time = time.time()
        
        result = self.stream_output(self.client.generate(prompt, system=SYSTEM_PROMPT))
        
        elapsed = time.time() - self.start_time
        self.print_status(f"Documentation generated in {elapsed:.1f}s", "success")
        
        return result
    
    def convert(self, filepath: str, target: str = "python", save_output: bool = True) -> str:
        """Convert COBOL to modern language."""
        self.print_header(f"COBOL to {target.upper()} Conversion")

        code = self.read_cobol_file(filepath)
        self.print_status(f"Loaded: {filepath} ({len(code)} characters)", "info")

        if target.lower() == "python":
            prompt = CONVERT_PYTHON_PROMPT.format(code=code)
        elif target.lower() == "java":
            prompt = CONVERT_JAVA_PROMPT.format(code=code)
        else:
            raise ValueError(f"Unsupported target language: {target}")

        self.print_subheader(f"Converted {target.upper()} Code")
        self.start_time = time.time()

        result = self.stream_output(self.client.generate(prompt, system=SYSTEM_PROMPT))

        elapsed = time.time() - self.start_time
        self.print_status(f"Conversion completed in {elapsed:.1f}s", "success")

        # Save converted code to target directory
        if save_output:
            target_dir, saved_files = self.save_converted_code(filepath, target.lower(), result)
            self.print_status(f"Output saved to: {target_dir}/", "success")
            for f in saved_files:
                self.print_status(f"  - {f.name}", "info")

        return result
    
    def generate_tests(self, filepath: str) -> str:
        """Generate test suite for COBOL program."""
        self.print_header("Test Suite Generation")
        
        code = self.read_cobol_file(filepath)
        self.print_status(f"Loaded: {filepath} ({len(code)} characters)", "info")
        
        prompt = TEST_GENERATION_PROMPT.format(code=code)
        
        self.print_subheader("Generated Test Suite")
        self.start_time = time.time()
        
        result = self.stream_output(self.client.generate(prompt, system=SYSTEM_PROMPT))
        
        elapsed = time.time() - self.start_time
        self.print_status(f"Test generation completed in {elapsed:.1f}s", "success")
        
        return result
    
    def full_pipeline(self, filepath: str, target: str = "python") -> dict:
        """Run complete modernization pipeline."""
        self.print_header("Full Modernization Pipeline")
        print(f"{Colors.BOLD}Target Language: {target.upper()}{Colors.ENDC}")
        print(f"{Colors.BOLD}Source File: {filepath}{Colors.ENDC}")
        
        results = {}
        total_start = time.time()
        
        # Step 1: Analysis
        print(f"\n{Colors.BOLD}[Step 1/4] Analyzing COBOL Program...{Colors.ENDC}")
        results['analysis'] = self.analyze(filepath)
        
        # Step 2: Documentation
        print(f"\n{Colors.BOLD}[Step 2/4] Generating Documentation...{Colors.ENDC}")
        results['documentation'] = self.document(filepath)
        
        # Step 3: Conversion
        print(f"\n{Colors.BOLD}[Step 3/4] Converting to {target.upper()}...{Colors.ENDC}")
        results['conversion'] = self.convert(filepath, target)
        
        # Step 4: Test Generation
        print(f"\n{Colors.BOLD}[Step 4/4] Generating Test Suite...{Colors.ENDC}")
        results['tests'] = self.generate_tests(filepath)
        
        total_elapsed = time.time() - total_start
        
        self.print_header("Pipeline Complete")
        print(f"{Colors.GREEN}Total processing time: {total_elapsed:.1f}s{Colors.ENDC}")
        print(f"\n{Colors.BOLD}Summary:{Colors.ENDC}")
        print(f"  • Analysis: Complete")
        print(f"  • Documentation: Complete")
        print(f"  • {target.upper()} Conversion: Complete")
        print(f"  • Test Suite: Complete")
        
        return results

# =============================================================================
# CLI Interface
# =============================================================================

def main():
    parser = argparse.ArgumentParser(
        description="Essential AI Rnj-1 COBOL Modernization Demo",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s analyze app/cbl/CBACT01C.cbl
  %(prog)s document app/cbl/CBACT01C.cbl
  %(prog)s convert app/cbl/CBACT01C.cbl --target python
  %(prog)s convert app/cbl/CBACT01C.cbl --target java
  %(prog)s test app/cbl/CBACT01C.cbl
  %(prog)s full app/cbl/CBACT01C.cbl --target python

For bank CTO demonstrations, use 'full' to show complete pipeline.
        """
    )
    
    parser.add_argument(
        'command',
        choices=['analyze', 'document', 'convert', 'test', 'full'],
        help='Command to execute'
    )
    
    parser.add_argument(
        'file',
        help='Path to COBOL source file'
    )
    
    parser.add_argument(
        '--target', '-t',
        choices=['python', 'java'],
        default='python',
        help='Target language for conversion (default: python)'
    )
    
    parser.add_argument(
        '--output', '-o',
        help='Output file path (default: stdout)'
    )
    
    parser.add_argument(
        '--model', '-m',
        default=MODEL_NAME,
        help=f'Ollama model to use (default: {MODEL_NAME})'
    )
    
    args = parser.parse_args()
    
    # Banner
    print(f"""
{Colors.BOLD}{Colors.CYAN}
╔═══════════════════════════════════════════════════════════════════════╗
║                                                                       ║
║   Essential AI - Rnj-1 COBOL Modernization Demo                       ║
║   Sovereign AI for Enterprise Legacy Modernization                    ║
║                                                                       ║
║   • 100% Local Inference - No Cloud Dependencies                      ║
║   • Zero Data Exfiltration - Your Code Never Leaves Your Network      ║
║   • 8B Parameters - Runs on Standard Hardware                         ║
║                                                                       ║
╚═══════════════════════════════════════════════════════════════════════╝
{Colors.ENDC}""")
    
    # Initialize
    modernizer = COBOLModernizer()
    
    # Override model if specified
    if args.model != MODEL_NAME:
        modernizer.client.model = args.model
    
    # Check prerequisites
    if not modernizer.check_prerequisites():
        sys.exit(1)
    
    # Execute command
    try:
        if args.command == 'analyze':
            result = modernizer.analyze(args.file)
        elif args.command == 'document':
            result = modernizer.document(args.file)
        elif args.command == 'convert':
            result = modernizer.convert(args.file, args.target)
        elif args.command == 'test':
            result = modernizer.generate_tests(args.file)
        elif args.command == 'full':
            results = modernizer.full_pipeline(args.file, args.target)
            result = json.dumps({k: v[:500] + "..." for k, v in results.items()}, indent=2)
        
        # Write to file if output specified
        if args.output:
            with open(args.output, 'w') as f:
                if isinstance(result, dict):
                    json.dump(result, f, indent=2)
                else:
                    f.write(result)
            print(f"\n{Colors.GREEN}Output saved to: {args.output}{Colors.ENDC}")
            
    except FileNotFoundError as e:
        print(f"{Colors.RED}Error: {e}{Colors.ENDC}")
        sys.exit(1)
    except ConnectionError as e:
        print(f"{Colors.RED}Error: {e}{Colors.ENDC}")
        sys.exit(1)
    except KeyboardInterrupt:
        print(f"\n{Colors.YELLOW}Interrupted by user{Colors.ENDC}")
        sys.exit(130)

if __name__ == "__main__":
    main()
