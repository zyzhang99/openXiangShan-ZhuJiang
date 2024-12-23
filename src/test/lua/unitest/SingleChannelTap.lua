local env = require "env"
local chi = require "CHI"
local bit = require "bit"

local dflit = [[
    | valid
    | ready
    | TraceTag
    | RetToSrc
    | DoNotGoToSD
    | NSE
    | NS
    | Addr
    | Opcode
    | FwdTxnID
    | TxnID
    | SrcID
    | TgtID
    | QoS
    ]]

local vflit = [[
    | valid
    | TraceTag
    | RetToSrc
    | DoNotGoToSD
    | NSE
    | NS
    | Addr
    | Opcode
    | FwdTxnID
    | TxnID
    | SrcID
    | TgtID
    | QoS
    ]]

local rsvd = [[
    | valid
    | bits
    ]]

local rx_flit_bdl = (vflit):bundle {hier = cfg.top, prefix = "io_in_flit_", is_decoupled = true}
local tx_flit_bdl = (vflit):bundle {hier = cfg.top, prefix = "io_out_flit_", is_decoupled = true}
local ij_bdl = (dflit):bundle {hier = cfg.top, prefix = "io_inject_", is_decoupled = true}
local ej_bdl = (dflit):bundle {hier = cfg.top, prefix = "io_eject_", is_decoupled = true}
local rx_rsvd_bdl = (rsvd):bundle {hier = cfg.top, prefix = "io_in_rsvd_", is_decoupled = false}
local tx_rsvd_bdl = (rsvd):bundle {hier = cfg.top, prefix = "io_out_rsvd_", is_decoupled = false}

local src_nid = 0xa8
local match_tag = src_nid

local function set_flit (this, src, tgt)
    this.valid:set(1)
    this.bits.TraceTag:set(0)
    this.bits.RetToSrc:set(0)
    this.bits.DoNotGoToSD:set(0)
    this.bits.NSE:set(0)
    this.bits.NS:set(0)
    this.bits.Addr:set(0, true)
    this.bits.Opcode:set(0)
    this.bits.FwdTxnID:set(0)
    this.bits.TxnID:set(0)
    this.bits.SrcID:set(src)
    this.bits.TgtID:set(tgt)
    this.bits.QoS:set(0)
end

local function chk_flit (this, src, tgt)
    this.valid:expect(1)
    this.bits.SrcID:expect(src)
    this.bits.TgtID:expect(tgt)
end

local function set_rsvd (this, b)
    this.valid:set(1)
    this.bits:set(b)
end

local function chk_rsvd (this, b)
    this.valid:expect(1)
    this.bits:expect(b)
end

local dut_state = dut.u_SingleChannelTapLocalSNP.state
local function chk_state (state)
    dut_state:expect(state)
end

local function change_to_rsvd_state()
        ij_bdl:set_flit(src_nid, 0x01)
        rx_flit_bdl:set_flit(0x77, 0x11)
        rx_rsvd_bdl:set_rsvd(0x16)
        chk_state(0x1)
    env.negedge(129)
        chk_state(0x2)
        tx_flit_bdl:chk_flit(0x77, 0x11)
        tx_rsvd_bdl:chk_rsvd(0x16)
        ij_bdl.valid:set(0)
        rx_flit_bdl.valid:set(0)
        rx_rsvd_bdl.valid:set(0)
end

local function change_to_wait_state()
        ij_bdl:set_flit(src_nid, 0x01)
        rx_flit_bdl:set_flit(0x77, 0x11)
        rx_rsvd_bdl:set_rsvd(0x16)
        chk_state(0x1)
    env.negedge(129)
        chk_state(0x2)
        tx_flit_bdl:chk_flit(0x77, 0x11)
        tx_rsvd_bdl:chk_rsvd(0x16)
    env.negedge(10)
        chk_state(0x2)
        tx_flit_bdl:chk_flit(0x77, 0x11)
        tx_rsvd_bdl:chk_rsvd(0x16)
        rx_rsvd_bdl.valid:set(0)
    env.negedge()
        chk_state(0x4)
        tx_rsvd_bdl:chk_rsvd(match_tag)
        tx_flit_bdl:chk_flit(0x77, 0x11)
        ij_bdl.valid:set(0)
        rx_flit_bdl.valid:set(0)
end

rx_flit_bdl.set_flit = set_flit
tx_flit_bdl.chk_flit = chk_flit

rx_rsvd_bdl.set_rsvd = set_rsvd
tx_rsvd_bdl.chk_rsvd = chk_rsvd

ij_bdl.set_flit = set_flit
ej_bdl.chk_flit = chk_flit

local function my_reset ()
    env.dut_reset()
    dut.io_matchTag:set(match_tag)
    dut.io_tapIdx:set(0)
    env.negedge()
    dut.io_matchTag:expect(match_tag)
    dut.io_tapIdx:expect(0)
end

local test_main = env.register_test_case "test_main" {
    function ()
        my_reset()
        env.negedge()
        
        -- 1. inject to empty slot
            ij_bdl:set_flit(src_nid, 0x01)
        env.negedge()
            ij_bdl.valid:set(0)

            ij_bdl.ready:expect(1)
            tx_flit_bdl:chk_flit(src_nid, 0x01)

        -- 2. inject and eject at the same cycle
            ej_bdl.ready:set(1)

            ij_bdl:set_flit(src_nid, 0x02)
            rx_flit_bdl:set_flit(0x7a, bit.bor(src_nid, 0x2))
        env.negedge()
            ij_bdl.valid:set(0)
            rx_flit_bdl.valid:set(0)
            ej_bdl.ready:set(0)

            ij_bdl.ready:expect(1)
            ej_bdl:chk_flit(0x7a, bit.bor(src_nid, 0x2))
            tx_flit_bdl:chk_flit(src_nid, 0x02)

        -- 3. suppress by ready of eject 
            ej_bdl.ready:set(0)

            ij_bdl:set_flit(src_nid, 0x02)
            rx_flit_bdl:set_flit(0x7a, bit.bor(src_nid, 0x2))
        env.negedge()
            ij_bdl.valid:set(0)
            rx_flit_bdl.valid:set(0)
            ej_bdl.ready:set(0)

            ij_bdl.ready:expect(0)
            tx_flit_bdl:chk_flit(0x7a, bit.bor(src_nid, 0x2))

        -- 4. suppress by rsvd 
            ej_bdl.ready:set(1)

            ij_bdl:set_flit(src_nid, 0x02)
            rx_flit_bdl:set_flit(0x7a, bit.bor(src_nid, 0x3))
            rx_rsvd_bdl:set_rsvd(0x57)
        env.negedge()
            ij_bdl.valid:set(0)
            rx_flit_bdl.valid:set(0)
            ej_bdl.ready:set(0)
            rx_rsvd_bdl.valid:set(0)

            ij_bdl.ready:expect(0)
            ej_bdl:chk_flit(0x7a, bit.bor(src_nid, 0x3))
            tx_flit_bdl.valid:expect(0)
            tx_rsvd_bdl:chk_rsvd(0x57)

        my_reset()

        -- 5. do reservation, inject and eject at the same cycle
        change_to_rsvd_state()
            ej_bdl.ready:set(1)

            ij_bdl:set_flit(src_nid, 0x02)
            rx_flit_bdl:set_flit(0x7a, bit.bor(src_nid, 0x3))
        env.negedge()
            chk_state(0x1)
            ij_bdl.valid:set(0)
            rx_flit_bdl.valid:set(0)
            ej_bdl.ready:set(0)

            ij_bdl.ready:expect(1)
            ej_bdl:chk_flit(0x7a, bit.bor(src_nid, 0x3))
            tx_flit_bdl:chk_flit(src_nid, 0x02)

        -- 6. try to inject to normal empty slot when state is wait
        change_to_wait_state()
            ij_bdl:set_flit(src_nid, 0x01)
        env.negedge()
            ij_bdl.valid:set(0)

            ij_bdl.ready:expect(0)
            tx_flit_bdl.valid:expect(0)

        -- 7. try to inject to empty slot with incorrect rsvd id when state is wait
            ij_bdl:set_flit(src_nid, 0x01)
            rx_rsvd_bdl:set_rsvd(0x47)
        env.negedge()
            ij_bdl.valid:set(0)
            rx_rsvd_bdl.valid:set(0)

            ij_bdl.ready:expect(0)
            tx_flit_bdl.valid:expect(0)
            tx_rsvd_bdl:chk_rsvd(0x47)

        -- 8. try to inject to empty slot with correct rsvd id when state is wait
            ij_bdl:set_flit(src_nid, 0x01)
            ej_bdl.ready:set(1)

            rx_rsvd_bdl:set_rsvd(match_tag)
            rx_flit_bdl:set_flit(0x7a, bit.bor(src_nid, 0x3))
        env.negedge()
            chk_state(0x1)
            ij_bdl.valid:set(0)
            rx_rsvd_bdl.valid:set(0)
            ej_bdl.ready:set(0)

            ij_bdl.ready:expect(1)
            tx_flit_bdl:chk_flit(src_nid, 0x01)
            ej_bdl:chk_flit(0x7a, bit.bor(src_nid, 0x3))
            tx_rsvd_bdl.valid:expect(0)
        env.negedge()
    end
}

fork {
    function ()
        sim.dump_wave()
        test_main()
        env.negedge(1024)
        env.TEST_SUCCESS()
    end
}