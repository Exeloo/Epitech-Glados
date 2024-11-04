# Glados Java2Script

Welcome to the Glados Language project! This project includes a custom programming language called **Glados** with two main components:
- **Glados Compiler** (`glados`): Compiles Glados source code.
- **Glados Virtual Machine (VM)** (`vm`): Executes compiled Glados code.

This project is structured as a Haskell Stack project, with all dependencies managed by Stack.

## Project Structure

- **`compiler/`**: Contains the source code for the Glados compiler.
- **`executer/`**: Contains the source code for the Glados VM.
- **`compiler/test/`**: Contains test scripts for running functional tests.
- **`Makefile`**: Provides various commands to build, clean, test, and manage the project.

## Requirements

To get started, make sure you have:
- [Haskell Stack](https://docs.haskellstack.org/) installed
- `Make` utility

## Installation and Usage

### Building the Project

To build both the compiler and the VM, run:

```bash
make all
```

This will:
1. Build the Glados compiler executable (`glados`) and place it in the project root.
2. Build the Glados VM executable (`vm`) and place it in the project root.

Alternatively, you can build them separately:
```bash
make compiler   # Builds only the Glados compiler
make executer   # Builds only the Glados VM
```

### Cleaning the Project

To remove build artifacts and binaries, use:

```bash
make clean      # Removes Stack build artifacts
make fclean     # Removes build artifacts and compiled executables
```

### Rebuilding the Project

To clean and rebuild everything from scratch, use:

```bash
make re
```

## Testing

This project includes both unit and functional tests.

- **Unit Tests**: Run with `make unit`, which uses Stack's test framework.
- **Functional Tests**: Run with `make functional`, which executes scripts located in `compiler/test/`.

To run all tests, use:

```bash
make tests
```

### Code Coverage

To generate a test coverage report, use:

```bash
make coverage
```

This command will generate a coverage report and attempt to open it in your default web browser. If no report is found, it will display an error message.

## Linting

To lint the code, use:

```bash
make lint
```

This command uses `hlint` to check for style issues in both the `compiler/lib` and `executer/lib` directories.

## Available Commands

| Command        | Description                                     |
|----------------|-------------------------------------------------|
| `make all`     | Builds both the compiler and VM.                |
| `make compiler`| Builds only the Glados compiler.                |
| `make executer`| Builds only the Glados VM.                      |
| `make clean`   | Removes build artifacts.                        |
| `make fclean`  | Removes build artifacts and executables.        |
| `make re`      | Cleans and rebuilds everything.                 |
| `make unit`    | Runs unit tests.                                |
| `make functional` | Runs functional tests.                      |
| `make tests`   | Runs both unit and functional tests.            |
| `make coverage`| Generates a code coverage report.               |
| `make lint`    | Lints the codebase using `hlint`.               |

---

**EPITECH Project, 2024**