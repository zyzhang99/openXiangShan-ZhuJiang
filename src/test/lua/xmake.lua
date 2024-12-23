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
    set_values("cfg.build_dir_name", "TestTLUL2ChiBridge")

target("TestEb")
    add_rules("verilua")
    add_toolchains("@vcs")

    add_files(
        build_dir .. "/Eb/*.sv",
        "./common/*.lua"
    )

    set_values("cfg.top", "EjectBufferSNP")
    set_values("cfg.lua_main", "./unitest/EjectBuffer.lua")
    set_values("cfg.build_dir_name", "TestEb")

target("TestSct")
    add_rules("verilua")
    add_toolchains("@vcs")

    add_files(
        build_dir .. "/Sct/*.sv",
        "./common/*.lua"
    )

    set_values("cfg.top", "SingleChannelTapLocalSNP")
    set_values("cfg.lua_main", "./unitest/SingleChannelTap.lua")
    set_values("cfg.build_dir_name", "TestSct")

task("verdi", function ()
  set_menu {
    usage = "xmake verdi [options]",
    description = "Display waveform with verdi",
    options = {
      {'c', "case", "kv", nil, "case name"},
    }
  }

  on_run(function ()
    import("core.base.option")
    assert(option.get("case"))
    local sim_dir = path.join(os.curdir(), "build", "vcs")
    local case = option.get("case")
    sim_dir = path.join(sim_dir, option.get("case"))

    os.cd(sim_dir)
    local cmds = "verdi -ssf test.vcd.fsdb"
    io.writefile("verdi.sh", cmds)
    print(cmds)
    os.execv(os.shell(), { "verdi.sh" })
    os.rm("verdi.sh")
    end)
end)