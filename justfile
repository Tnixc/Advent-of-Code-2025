# Default recipe - show available commands
default:
    @just --list

# Compile a specific day (e.g., just build 1)
build day:
    @echo "Building day {{day}}..."
    @mkdir -p target
    cppo -n {{day}}.ml > target/{{day}}_gen.ml
    cd target && ocamlopt -o {{day}} {{day}}_gen.ml
    @rm target/{{day}}_gen.ml

# Run a specific day (e.g., just run 1)
run day: (build day)
    @echo "Running day {{day}}..."
    @./target/{{day}}

# Compile and run in one step (e.g., just solve 1)
solve day: (run day)

# Clean compiled files
clean:
    rm -rf target

# Build all days (finds all *.ml files)
build-all:
    #!/usr/bin/env bash
    mkdir -p target
    for file in [0-9].ml [0-9][0-9].ml; do
        if [ -f "$file" ]; then
            day="${file%.ml}"
            echo "Building day $day..."
            cppo -n "$file" > "target/${day}_gen.ml"
            (cd target && ocamlopt -o "$day" "${day}_gen.ml") || exit 1
            rm "target/${day}_gen.ml"
        fi
    done

# Watch and rebuild when files change (requires entr)
watch day:
    ls {{day}}.ml inputs/{{day}} 2>/dev/null | entr -c just run {{day}}
