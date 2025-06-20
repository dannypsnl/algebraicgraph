import Lake
open Lake DSL

package «algebraicgraph» {
  -- add package configuration options here
}

@[default_target]
lean_lib «Algebraicgraph» {
  -- add library configuration options here
}

require batteries from git "https://github.com/leanprover/std4.git" @"main"
