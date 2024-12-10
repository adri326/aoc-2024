BUILD_DIR = build
SRC_DIR = src

GO_DAYS = $(wildcard src/*/main.go)
GO_OUTPUT = $(GO_DAYS:src/%/main.go=$(BUILD_DIR)/%)
ALL_OUTPUT = $(GO_OUTPUT)

OCAML_DAYS = $(wildcard src/day*.ml)
OCAML_OUTPUT = $(OCAML_DAYS:src/%.ml=$(BUILD_DIR)/%)
ALL_OUTPUT += $(OCAML_OUTPUT)

ZIG_DAYS = $(wildcard src/day*.zig)
ZIG_OUTPUT = $(ZIG_DAYS:src/%.zig=$(BUILD_DIR)/%)
ALL_OUTPUT += $(ZIG_OUTPUT)

ADA_DAYS = $(wildcard src/day*.adb)
ADA_OUTPUT = $(ADA_DAYS:src/%.adb=$(BUILD_DIR)/%)
ALL_OUTPUT += $(ADA_OUTPUT)

all: $(ALL_OUTPUT)
.PHONY: all

$(BUILD_DIR)/:
	@mkdir -p $(BUILD_DIR)/

$(GO_OUTPUT): $(BUILD_DIR)/%: $(SRC_DIR)/%/main.go | $(BUILD_DIR)/
	@if command -v go >/dev/null; then \
	echo "Compiling $@"; \
	go build -o $@ $<; \
	else echo "No go toolchain available (go), skipping $@"; fi

$(OCAML_OUTPUT): $(BUILD_DIR)/%: $(SRC_DIR)/%.ml | $(BUILD_DIR)/
	@if command -v dune >/dev/null; then \
	echo "Compiling $@"; \
	dune build $<; \
	cp _build/default/src/$*.exe $@; chmod +w $@; \
	else echo "No ocaml toolchain available (dune), skipping $@"; fi

$(ZIG_OUTPUT): $(BUILD_DIR)/%: $(SRC_DIR)/%.zig | $(BUILD_DIR)/
	@if command -v zig >/dev/null; then \
	echo "Compiling $@"; \
	zig build $* -Doptimize=ReleaseFast --prefix-exe-dir . --prefix build --summary none; \
	else echo "No zig toolchain available (zig), skipping $@"; fi

$(ADA_OUTPUT): $(BUILD_DIR)/%: $(SRC_DIR)/%.adb | $(BUILD_DIR)/
	@if command -v gprbuild >/dev/null; then \
	echo "Compiling $@"; \
	gprbuild $*; \
	else echo "No ada toolchain available (gprbuild), skipping $@"; fi
