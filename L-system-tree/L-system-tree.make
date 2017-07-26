
%.stl: %.scad
	openscad -o $@ $^

%-trunk.obj: %-trunk.stl
	$(WAVEFRONT_OBJ_TOOL) -n -C $(COMBINE_DISTANCE) -c $^ -o $@ --color 0.65 0.16 0.16

%-leaves.obj: %-leaves.stl
	$(WAVEFRONT_OBJ_TOOL) -n -C $(COMBINE_DISTANCE) -c $^ -o $@ --color 0 1 0

# %.obj: %.stl
# 	$(WAVEFRONT_OBJ_TOOL) -n -C $(COMBINE_DISTANCE) -c $^ -o $@

# %.obj: %-trunk.obj %-leaves.obj
# 	$(WAVEFRONT_OBJ_TOOL) -n -C $(COMBINE_DISTANCE) -o $@ $^


# ---

TREE0_W=5
TREE0_SHAPE=iixxyiO

tree0-leaves.scad: $(L_SYSTEM_TREE)
	$(L_SYSTEM_TREE) --fn $(OPENSCAD_FN) -w $(TREE0_W) -t $(TREE0_SHAPE) --skip-trunk -o $@

tree0-trunk.scad: $(L_SYSTEM_TREE) tree0-leaves.scad
	$(L_SYSTEM_TREE) --fn $(OPENSCAD_FN) -w $(TREE0_W) -t $(TREE0_SHAPE) --skip-leaves -o $@ -d tree0-leaves.scad

tree0-hull.scad: $(L_SYSTEM_TREE)
	$(L_SYSTEM_TREE) --fn $(OPENSCAD_FN) -w $(TREE0_W) -t $(TREE0_SHAPE) --hull -o tree0-hull.scad

tree0-hull.obj: tree0-hull.stl
	$(WAVEFRONT_OBJ_TOOL) -n -c -o $@ $^

tree0.obj: tree0-trunk.obj tree0-leaves.obj
	$(WAVEFRONT_OBJ_TOOL) -n -c -C $(COMBINE_DISTANCE) -o $@ $^

# ---

TREE1_W=3
TREE1_SHAPE=iixyyyiO

tree1-leaves.scad: $(L_SYSTEM_TREE)
	$(L_SYSTEM_TREE) --fn $(OPENSCAD_FN) -w $(TREE1_W) -t $(TREE1_SHAPE) --skip-trunk -o $@

tree1-trunk.scad: $(L_SYSTEM_TREE) tree1-leaves.scad
	$(L_SYSTEM_TREE) --fn $(OPENSCAD_FN) -w $(TREE1_W) -t $(TREE1_SHAPE) --skip-leaves -o $@ -d tree1-leaves.scad

tree1-hull.scad: $(L_SYSTEM_TREE)
	$(L_SYSTEM_TREE) --fn $(OPENSCAD_FN) -w $(TREE1_W) -t $(TREE1_SHAPE) --hull -o tree1-hull.scad

tree1-hull.obj: tree1-hull.stl
	$(WAVEFRONT_OBJ_TOOL) -n -c -o $@ $^

tree1.obj: tree1-trunk.obj tree1-leaves.obj
	$(WAVEFRONT_OBJ_TOOL) -n -c -C $(COMBINE_DISTANCE) -o $@ $^


# ---

TREE2_W=4
TREE2_SHAPE=iixyxxiO

tree2-leaves.scad: $(L_SYSTEM_TREE)
	$(L_SYSTEM_TREE) --fn $(OPENSCAD_FN) -w $(TREE2_W) -t $(TREE2_SHAPE) --skip-trunk -o $@

tree2-trunk.scad: $(L_SYSTEM_TREE) tree2-leaves.scad
	$(L_SYSTEM_TREE) --fn $(OPENSCAD_FN) -w $(TREE2_W) -t $(TREE2_SHAPE) --skip-leaves -o $@ -d tree2-leaves.scad

tree2-hull.scad: $(L_SYSTEM_TREE)
	$(L_SYSTEM_TREE) --fn $(OPENSCAD_FN) -w $(TREE2_W) -t $(TREE2_SHAPE) --hull -o tree2-hull.scad

tree2-hull.obj: tree2-hull.stl
	$(WAVEFRONT_OBJ_TOOL) -n -c -o $@ $^

tree2.obj: tree2-trunk.obj tree2-leaves.obj
	$(WAVEFRONT_OBJ_TOOL) -n -c -C $(COMBINE_DISTANCE) -o $@ $^


# ---

TREE3_W=5
TREE3_SHAPE=iifxyiO

tree3-leaves.scad: $(L_SYSTEM_TREE)
	$(L_SYSTEM_TREE) --fn $(OPENSCAD_FN) -w $(TREE3_W) -t $(TREE3_SHAPE) --skip-trunk -o $@

tree3-trunk.scad: $(L_SYSTEM_TREE) tree3-leaves.scad
	$(L_SYSTEM_TREE) --fn $(OPENSCAD_FN) -w $(TREE3_W) -t $(TREE3_SHAPE) --skip-leaves -o $@ -d tree3-leaves.scad

tree3-hull.scad: $(L_SYSTEM_TREE)
	$(L_SYSTEM_TREE) --fn $(OPENSCAD_FN) -w $(TREE3_W) -t $(TREE3_SHAPE) --hull -o tree3-hull.scad

tree3-hull.obj: tree3-hull.stl
	$(WAVEFRONT_OBJ_TOOL) -n -c -o $@ $^

tree3.obj: tree3-trunk.obj tree3-leaves.obj
	$(WAVEFRONT_OBJ_TOOL) -n -c -C $(COMBINE_DISTANCE) -o $@ $^
