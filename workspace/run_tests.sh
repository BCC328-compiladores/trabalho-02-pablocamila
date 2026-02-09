MODE=${1:-all}

run_stage1() {
    echo "--- Lexer ---"
    cabal run -v0 bcc328 -- --lexer "$1"
    echo "--- Parser ---"
    cabal run -v0 bcc328 -- --parser "$1"
    echo "--- Pretty ---"
    cabal run -v0 bcc328 -- --pretty "$1"
}

run_check() {
    echo "--- Type Check ---"
    cabal run -v0 bcc328 -- --check "$1"
}

export PATH=$HOME/.local/bin:$PATH
export LIBRARY_PATH=$HOME/lib:$LIBRARY_PATH

echo "Running mode: $MODE"

for file in tests/*.sl; do
    echo "!------------ Testing $file ----------------------!"

    case $MODE in
        stage1)
            run_stage1 "$file"
            ;;
        check)
            run_check "$file"
            ;;
        all)
            run_stage1 "$file"
            run_check "$file"
            ;;
        *)
            echo "Error: Unknown mode '$MODE'"
            echo "Usage: $0 [stage1|check|all]"
            exit 1
            ;;
    esac

    echo "---------------------------------------------------"
done