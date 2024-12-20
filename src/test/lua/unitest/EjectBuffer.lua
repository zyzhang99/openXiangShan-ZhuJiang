local env = require "env"
local chi = require "CHI"

local buf_size = 5

local flit = [[
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

local function set_tag (this, tag)
    this.bits.TraceTag:set(0)
    this.bits.RetToSrc:set(0)
    this.bits.DoNotGoToSD:set(0)
    this.bits.NSE:set(0)
    this.bits.NS:set(0)
    this.bits.Addr:set(0, true)
    this.bits.Opcode:set(0)
    this.bits.FwdTxnID:set(0)
    this.bits.TxnID:set(tag)
    this.bits.SrcID:set(tag)
    this.bits.TgtID:set(0)
    this.bits.QoS:set(0)
end

local function chk_tag (this, tag)
    this.bits.TxnID:expect(tag)
    this.bits.SrcID:expect(tag)
end

local enq_bdl = (flit):bundle {hier = cfg.top, prefix = "io_enq_", is_decoupled = true}
local deq_bdl = (flit):bundle {hier = cfg.top, prefix = "io_deq_", is_decoupled = true}
enq_bdl.set_tag = set_tag
deq_bdl.set_tag = set_tag
enq_bdl.chk_tag = chk_tag
deq_bdl.chk_tag = chk_tag

local test_main = env.register_test_case "test_main" {
    function ()
        local function enq_chk (tag, rdy)
                enq_bdl.valid:set(1)
                enq_bdl:set_tag(tag)
            env.posedge()
                enq_bdl.ready:expect(rdy)
            env.negedge()
                enq_bdl.valid:set(0)
        end

        local function deq_chk (tag)
                deq_bdl.ready:set(1)
                deq_bdl.valid:expect(1)
                deq_bdl:chk_tag(tag)
            env.negedge()
                deq_bdl.ready:set(0)
        end

        local function pass_through (tag)
            enq_chk(tag, 1)
            env.negedge()
            deq_chk(tag)
        end

        local function buf_size_chk (size, tag_off)
            for i = 0, buf_size - 1, 1 do
                local tag = i + tag_off
                if(i < size) then 
                    enq_chk(tag, 1)
                else
                    enq_bdl:set_tag(tag)
                    env.posedge()
                    enq_bdl.ready:expect(0)
                    env.negedge()
                end
            end
            env.negedge()
            for i = 0, buf_size - 1, 1 do
                local tag = i + tag_off
                if(i < size) then 
                    deq_chk(tag)
                else
                    deq_bdl.valid:expect(0)
                    env.negedge()
                end
            end
        end

        enq_bdl.valid:set(0)
        deq_bdl.ready:set(0)
        env.dut_reset()
        env.negedge()

        -- Fill up the buffer, size of buffer is 5
        for i = 1, 5, 1 do enq_chk(i, 1) end

        -- Try to reserve 6 flit, tags are 6, 7, 8, 9, 10, 11. 11 is no supposed to be reserved.
        for i = 6, 11, 1 do enq_chk(i, 0) end

        -- deque 1 flit
        deq_chk(1)

        -- try to enque several flits
        enq_chk(12, 0)
        enq_chk(11, 0)
        enq_chk(10, 1)
        enq_chk(11, 0)

        -- Now, 2, 3, 4, 5, 10 are in the buffer, while 6, 7, 8, 9, 11 are reserved
        -- Deque all flit and try to enqueue flits not reserved
        enq_chk(12, 0)
        deq_chk(2)
        enq_chk(12, 0)
        deq_chk(3)
        enq_chk(12, 0)
        deq_chk(4)
        enq_chk(12, 0)
        deq_chk(5)
        enq_chk(12, 0)
        deq_chk(10)
        enq_chk(12, 0)
        enq_chk(11, 1)
        env.negedge()
        deq_chk(11)

        -- Now, buffer is empty, while 6, 7, 8, 9 are reserved
        enq_chk(12, 1)
        enq_chk(10, 0)
        deq_chk(12)

        -- Now, buffer is empty, while 6, 7, 8, 9, 10 are reserved
        buf_size_chk (0, 11)
        pass_through(6)
        buf_size_chk (1, 11)
        pass_through(7)
        buf_size_chk (2, 11)
        pass_through(8)
        buf_size_chk (3, 11)
        pass_through(9)
        buf_size_chk (4, 11)
        pass_through(10)
        buf_size_chk (5, 11)
    end
}
fork {
    function ()
        sim.dump_wave()
        test_main()
        env.negedge(100)
        env.TEST_SUCCESS()
    end
}