local build_dir = os.curdir() .. "/../../../build"

target("TestTLUL2ChiBridge")
    add_rules("verilua")
    add_toolchains("@vcs")
    
    add_files(
        build_dir .. "/TLUL2ChiBridge/*.sv",
        "./common/*.lua" 
    )

    set_values("cfg.top", "TLUL2ChiBridge")
    set_values("cfg.lua_main", "./unitest/TLUL2ChiBridge.lua")
