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

target("TestEb")
    add_rules("verilua")
    add_toolchains("@vcs")

    add_files(
        build_dir .. "/Eb/*.sv",
        "./common/*.lua"
    )

    set_values("cfg.top", "EjectBufferSNP")
    set_values("cfg.lua_main", "./unitest/EjectBuffer.lua")

target("TestSct")
    add_rules("verilua")
    add_toolchains("@vcs")

    add_files(
        build_dir .. "/Sct/*.sv",
        "./common/*.lua"
    )

    set_values("cfg.top", "SingleChannelTapLocalSNP")
    set_values("cfg.lua_main", "./unitest/SingleChannelTap.lua")