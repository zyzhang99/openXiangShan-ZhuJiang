local env = require "env"
local chi = require "CHI"
local tl = require "TileLink"

local OpcodeREQ = chi.OpcodeREQ
local OpcodeSNP = chi.OpcodeSNP
local OpcodeDAT = chi.OpcodeDAT
local OpcodeRSP = chi.OpcodeRSP
local CHIResp = chi.CHIResp
local CHIOrder = chi.CHIOrder

local TLMessageA = tl.TLMessageA
local TLMessageD = tl.TLMessageD

local tl_a = ([[
    | valid
    | ready
    | address
    | opcode
    | param
    | mask
    | source
    | data
]]):bundle {hier = cfg.top, prefix = "tlm_a_"}

tl_a.get = function (this, address, source, mask)
    env.negedge()
        this.valid:set(1)
        this.bits.opcode:set(TLMessageA.Get)
        this.bits.address:set(address, true)
        this.bits.source:set(source)
        this.bits.mask:set(mask)
        env.posedge()
        this.ready:expect(1)
    env.negedge()
        this.valid:set(0)
end

tl_a.putfull = function (this, address, source, data_str)
    env.negedge()
        this.valid:set(1)
        this.bits.opcode:set(TLMessageA.PutFullData)
        this.bits.address:set(address, true)
        this.bits.source:set(source)
        this.bits.mask:set(0xff)
        this.bits.data:set_str(data_str)
        env.posedge()
        this.ready:expect(1)
    env.negedge()
        this.valid:set(0)
end

tl_a.putpartial = function (this, address, source, data_str, mask)
    env.negedge()
        this.valid:set(1)
        this.bits.opcode:set(TLMessageA.PutPartialData)
        this.bits.address:set(address, true)
        this.bits.source:set(source)
        this.bits.mask:set(mask)
        this.bits.data:set_str(data_str)
        env.posedge()
        this.ready:expect(1)
    env.negedge()
        this.valid:set(0)
end

local tl_d = ([[
    | valid
    | ready
    | opcode
    | param
    | source
    | sink
    | data
]]):bundle {hier = cfg.top, prefix = "tlm_d_"}

local chi_txreq = ([[
    | valid
    | ready
    | ExpCompAck
    | Order
    | Addr
    | Size
    | Opcode
    | TxnID
]]):bundle {hier = cfg.top, prefix = "icn_tx_req_"}

local chi_txrsp = ([[
    | valid
    | ready
    | Resp
    | Opcode
    | DBID
    | TxnID
]]):bundle {hier = cfg.top, prefix = "icn_tx_resp_"}

local chi_txdat = ([[
    | valid
    | ready
    | Data
    | BE
    | DBID
    | DataID
    | TxnID
    | Opcode
]]):bundle {hier = cfg.top, prefix = "icn_tx_data_"}

local chi_rxrsp = ([[
    | valid
    | ready
    | Resp
    | Opcode
    | DBID
    | TxnID
]]):bundle {hier = cfg.top, prefix = "icn_rx_resp_"}

chi_rxrsp.readreceipt = function (this, txn_id)
    env.negedge()
        this.valid:set(1)
        this.bits.Opcode:set(OpcodeRSP.ReadReceipt)
        this.bits.TxnID:set(txn_id)
    env.negedge()
        this.valid:set(0)
end

chi_rxrsp.dbidresp = function (this, txn_id, db_id)
    env.negedge()
        chi_rxrsp.bits.TxnID:set(txn_id)
        chi_rxrsp.bits.Opcode:set(OpcodeRSP.DBIDResp)
        chi_rxrsp.bits.DBID:set(db_id)
        chi_rxrsp.valid:set(1)
    env.negedge()
        chi_rxrsp.valid:set(0)
end

chi_rxrsp.comp = function (this, txn_id, db_id)
    env.negedge()
        chi_rxrsp.bits.TxnID:set(txn_id)
        chi_rxrsp.bits.Opcode:set(OpcodeRSP.Comp)
        chi_rxrsp.bits.DBID:set(db_id)
        chi_rxrsp.valid:set(1)
    env.negedge()
        chi_rxrsp.valid:set(0)
end

local chi_rxdat = ([[
    | valid
    | ready
    | Opcode
    | Data
    | BE
    | DBID
    | DataID
    | TxnID
    | Resp
]]):bundle {hier = cfg.top, prefix = "icn_rx_data_"}

chi_rxdat.compdat = function (this, txn_id, data_str, db_id, resp)
    local resp = resp or CHIResp.I
    env.negedge()
        chi_rxdat.valid:set(1)
        chi_rxdat.bits.TxnID:set(txn_id)
        chi_rxdat.bits.DataID:set(0)
        chi_rxdat.bits.Opcode:set(OpcodeDAT.CompData)
        chi_rxdat.bits.Data:set_str(data_str)
        chi_rxdat.bits.DBID:set(db_id)
        chi_rxdat.bits.Resp:set(resp)
    env.negedge()
        chi_rxdat.valid:set(0)
end

local machineHandler = dut.u_TLUL2ChiBridge.machineHandler
local machines = {}
for i = 0, 15 do
    machines[i] = machineHandler["machines_" .. i]
end

local test_get = env.register_test_case "test_get" {
    function ()
        env.dut_reset()
        tl_d.ready:set(1); chi_txreq.ready:set(1); chi_txrsp.ready:set(1); chi_txdat.ready:set(1)

        -- Single Get
        do
            env.negedge()
                tl_a:get(0x1000, 0, 0xFF)
                
            env.expect_happen_until(10, function () return chi_txreq:fire() and chi_txreq.bits.Opcode:is(OpcodeREQ.ReadNoSnp) and chi_txreq.bits.Order:is(CHIOrder.EndpointOrder) end)
            
            local txn_id = chi_txreq.bits.TxnID:get()
            chi_rxrsp:readreceipt(chi_txreq.bits.TxnID:get())
            
            env.negedge(math.random(1, 10))
                chi_rxdat:compdat(txn_id, "0x1122", 0, CHIResp.I)
            
            env.expect_happen_until(10, function () return tl_d:fire() and tl_d.bits.opcode:is(TLMessageD.AccessAckData) and tl_d.bits.data:is_hex_str("0x1122") end)
            env.negedge()
            env.expect_not_happen_until(10, function () return tl_d:fire() end)

            machines[0].io_status_state:expect(0)

            env.negedge(100)
        end

        -- Multiple Get
        env.dut_reset()
        chi_txreq.ready:set(0)
        do
            local function check_req(txn_id) 
                env.expect_happen_until(10, function () return chi_txreq:fire() and chi_txreq.bits.Opcode:is(OpcodeREQ.ReadNoSnp) and chi_txreq.bits.TxnID:is(txn_id) end)
            end
            
            local function check_no_req()
                env.expect_not_happen_until(10, function () return chi_txreq:fire() end)
            end

            fork {
                function ()
                    env.expect_not_happen_until(100, function () return chi_txreq:fire() end)
                end
            }
            for i = 1, 16 do
                env.negedge()
                    tl_a.valid:set(1)
                    tl_a.bits.opcode:set(TLMessageA.Get)
                    tl_a.bits.mask:set(0xff)
                    tl_a.bits.address:set(0x1000 + (i - 1) * 0x100, true)
                    tl_a.bits.source:set(i - 1)
                    env.posedge()
                    tl_a.ready:expect(1)
                machineHandler.enqPtr:expect(i - 1)
            end
            env.negedge()
                tl_a.valid:set(0)
                env.posedge()
                tl_a.ready:expect(0)

            env.negedge(math.random(100, 110))
            chi_txreq.ready:set(1)

            machineHandler.issueReqPtr:expect(0)
            check_req(0)
            env.negedge(); machineHandler.issueReqPtr:expect(0)
            check_no_req()
            chi_rxrsp:readreceipt(0)

            check_req(1)
            env.negedge(); machineHandler.issueReqPtr:expect(1)
            check_no_req()
            
            chi_txreq.ready:set(0)
            chi_rxrsp:readreceipt(1)

            fork {
                function ()
                    check_no_req()
                end
            }

            chi_rxdat:compdat(0, "0x1122" , 1, CHIResp.I) -- txn_id = 0, db_id = 1
            env.expect_happen_until(10, function () return tl_d:fire() and tl_d.bits.data:is_hex_str("0x1122") end)
            env.negedge()
                machines[0].io_status_state:expect(0)
            
            chi_rxdat:compdat(1, "0x2233" , 1, CHIResp.I) -- txn_id = 1, db_id = 1
            env.expect_happen_until(10, function () return tl_d:fire() and tl_d.bits.data:is_hex_str("0x2233") end)
            env.negedge()
                machines[1].io_status_state:expect(0)

            env.negedge(10)

            -- Deal with the rest of the requests
            chi_txreq.ready:set(1)
            fork {
                function ()
                    for i = 2, 15 do
                        check_req(i)
                        env.negedge()
                    end
                    check_no_req()
                end,

                function ()
                    for i = 2, 15 do
                        local data_str = "0x3344" .. string.format("%02x", i)
                        chi_rxrsp:readreceipt(i)
                        chi_rxdat:compdat(i, data_str, 1, CHIResp.I) -- txn_id = i, db_id = 1
                        env.expect_happen_until(10, function () return tl_d:fire() and tl_d.bits.data:is_hex_str(data_str) end)
                        env.negedge()
                            machines[i].io_status_state:expect(0)
                    end
                end
            }

            env.negedge(200)
            for i = 1, 16 do
                machines[i - 1].io_status_state:expect(0)
            end
        end
    end
}

local test_put = env.register_test_case "test_put" {
    function ()
        env.dut_reset()
        tl_d.ready:set(1); chi_txreq.ready:set(1); chi_txrsp.ready:set(1); chi_txdat.ready:set(1)

        -- Single PutFull
        do
            env.negedge()
                tl_a:putfull(0x1000, 0, "0xdead")
                
            env.expect_happen_until(10, function () return chi_txreq:fire() and chi_txreq.bits.Opcode:is(OpcodeREQ.WriteNoSnpPtl) and chi_txreq.bits.Order:is(CHIOrder.OWO) end)
                        
            env.negedge(math.random(1, 10))
                chi_rxrsp:dbidresp(0, 4) -- txn_id = 0, db_id = 4
            env.expect_happen_until(10, function () return chi_txdat:fire() and chi_txdat.bits.TxnID:is(4) and chi_txdat.bits.BE:is(0xff) end)
                chi_rxrsp:comp(0, 4) -- txn_id = 0, db_id = 4
            env.expect_happen_until(10, function () return chi_txrsp:fire() and chi_txrsp.bits.Opcode:is(OpcodeRSP.CompAck) and chi_txrsp.bits.TxnID:is(4) end) 
            
            env.expect_happen_until(10, function () return tl_d:fire() and tl_d.bits.opcode:is(TLMessageD.AccessAck) end)
            env.negedge()
            env.expect_not_happen_until(10, function () return tl_d:fire() end)

            machines[0].io_status_state:expect(0)

            env.negedge(100)
        end

        -- Single PutPartial
        env.dut_reset()
        do
            env.negedge()
                tl_a:putpartial(0x1000, 0, "0xdead", 0xAA)
                
            env.expect_happen_until(10, function () return chi_txreq:fire() and chi_txreq.bits.Opcode:is(OpcodeREQ.WriteNoSnpPtl) and chi_txreq.bits.Order:is_not(0) end)
                        
            env.negedge(math.random(1, 10))
                chi_rxrsp:dbidresp(0, 4) -- txn_id = 0, db_id = 4
            env.expect_happen_until(10, function () return chi_txdat:fire() and chi_txdat.bits.TxnID:is(4) and chi_txdat.bits.BE:is(0xAA) end)
                chi_rxrsp:comp(0, 4) -- txn_id = 0, db_id = 4
            env.expect_happen_until(10, function () return chi_txrsp:fire() and chi_txrsp.bits.Opcode:is(OpcodeRSP.CompAck) and chi_txrsp.bits.TxnID:is(4) end) 
            
            env.expect_happen_until(10, function () return tl_d:fire() and tl_d.bits.opcode:is(TLMessageD.AccessAck) end)
            env.negedge()
            env.expect_not_happen_until(10, function () return tl_d:fire() end)

            machines[0].io_status_state:expect(0)

            env.negedge(100)
        end

        -- Multiple Put
        env.dut_reset()
        chi_txreq.ready:set(0)
        do
            local function check_req(txn_id) 
                env.expect_happen_until(10, function () return chi_txreq:fire() and chi_txreq.bits.Opcode:is(OpcodeREQ.WriteNoSnpPtl) and chi_txreq.bits.TxnID:is(txn_id) end)
            end
            
            local function check_no_req()
                env.expect_not_happen_until(10, function () return chi_txreq:fire() end)
            end

            local function check_ack(txn_id) 
                env.expect_happen_until(10, function () return chi_txrsp:fire() and chi_txrsp.bits.Opcode:is(OpcodeRSP.CompAck) and chi_txrsp.bits.TxnID:is(txn_id) end)
            end
            
            local function check_no_ack()
                env.expect_not_happen_until(10, function () return chi_txrsp:fire() end)
            end

            fork {
                function ()
                    env.expect_not_happen_until(100, function () return chi_txreq:fire() end)
                end
            }
            local data_hex_str_vec = {}
            local data_vec = {}
            for i = 1, 16 do
                local _data_str = "1122" .. string.format("%02x", i)
                local data_hex_str = "0x" .. _data_str
                env.negedge()
                    tl_a.valid:set(1)
                    tl_a.bits.opcode:set(TLMessageA.PutFullData)
                    tl_a.bits.mask:set(0xff)
                    tl_a.bits.address:set(0x1000 + (i - 1) * 0x100, true)
                    tl_a.bits.data:set_str(data_hex_str)
                    tl_a.bits.source:set(i - 1)
                    env.posedge()
                    tl_a.ready:expect(1)
                machineHandler.enqPtr:expect(i - 1)
                table.insert(data_hex_str_vec, data_hex_str)
                table.insert(data_vec, tonumber(_data_str, 16))
            end
            env.negedge()
                tl_a.valid:set(0)
                env.posedge()
                tl_a.ready:expect(0)
            
            env.negedge(math.random(100, 110))
            chi_txreq.ready:set(1)

            fork {
                function ()
                    env.expect_happen_until(15, function () return chi_txdat:fire() and chi_txdat.bits.TxnID:is(4) and chi_txdat.bits.Data:get()[1] == data_vec[1] end)
                    env.expect_happen_until(15, function () return chi_txdat:fire() and chi_txdat.bits.TxnID:is(5) and chi_txdat.bits.Data:get()[1] == data_vec[2] end)
                end,
                function ()
                    check_no_ack()
                end
            }
            machineHandler.issueReqPtr:expect(0)
            check_req(0)
            env.negedge(); machineHandler.issueReqPtr:expect(0)
            check_no_req()
            chi_rxrsp:dbidresp(0, 4) -- txn_id = 0, db_id = 4

            check_req(1)
            env.negedge(); machineHandler.issueReqPtr:expect(1)
            check_no_req()

            chi_txreq.ready:set(0)
            chi_rxrsp:dbidresp(1, 5) -- txn_id = 1, db_id = 5

            env.negedge(100)
            
            machineHandler.issueAckPtr:expect(0)
            chi_rxrsp:comp(0, 4)
            check_ack(4)
            env.expect_happen_until(10, function () return tl_d:fire() and tl_d.bits.opcode:is(TLMessageD.AccessAck) and tl_d.bits.sink:is(0) end)
            env.negedge()
                machines[0].io_status_state:expect(0)
            
            machineHandler.issueAckPtr:expect(1)
            chi_rxrsp:comp(1, 5)
            check_ack(5)
            env.expect_happen_until(10, function () return tl_d:fire() and tl_d.bits.opcode:is(TLMessageD.AccessAck) end)
            env.negedge()
                machines[1].io_status_state:expect(0)

            chi_txreq.ready:set(1)
            fork {
                function ()
                    for i = 2, 15 do
                        check_req(i)
                        env.negedge()
                    end
                    check_no_req()
                end,

                function ()
                    for i = 2, 15 do
                        chi_rxrsp:dbidresp(i, 5 + i) -- txn_id = 1, db_id = 5 + i
                        env.expect_happen_until(10, function () return chi_txdat:fire() and chi_txdat.bits.TxnID:is(5 + i) and chi_txdat.bits.Data:get()[1] == data_vec[i + 1] end)
                        chi_rxrsp:comp(i, 5 + i)
                        
                        env.expect_happen_until(10, function () return tl_d:fire() and tl_d.bits.sink:is(i) end)
                        env.negedge()
                            machines[i].io_status_state:expect(0)
                    end
                end
            }

            env.negedge(100)
        end
    end
}

local test_mix_get_put = env.register_test_case "test_mix_get_put" {
    function ()
        env.dut_reset()
        tl_d.ready:set(1); chi_txreq.ready:set(1); chi_txrsp.ready:set(1); chi_txdat.ready:set(1)

        -- Get and Put
        chi_txreq.ready:set(0)
        do
            env.negedge()
                tl_a:get(0x1000, 0, 0xFF)
            env.negedge()
                tl_a:putfull(0x1000, 0, "0xdead")
            
            chi_txreq.ready:set(1)
            env.expect_happen_until(10, function () return chi_txreq:fire() and chi_txreq.bits.Opcode:is(OpcodeREQ.ReadNoSnp) and chi_txreq.bits.TxnID:is(0) end)
            env.negedge()
            env.expect_not_happen_until(10, function () return chi_txreq:fire() end)
            
            chi_rxrsp:readreceipt(0)
            env.expect_happen_until(10, function () return chi_txreq:fire() and chi_txreq.bits.Opcode:is(OpcodeREQ.WriteNoSnpPtl) and chi_txreq.bits.TxnID:is(1) end)

            chi_rxdat:compdat(0, "0x1122", 0, CHIResp.I) -- txn_id = 0, db_id = 0
            env.expect_happen_until(10, function () return tl_d:fire() and tl_d.bits.opcode:is(TLMessageD.AccessAckData) and tl_d.bits.data:is_hex_str("0x1122") end)
            env.negedge()
                machines[0].io_status_state:expect(0)

            chi_rxrsp:dbidresp(1, 0) -- txn_id = 1, db_id = 0
            env.expect_happen_until(10, function () return chi_txdat:fire() and chi_txdat.bits.Opcode:is(OpcodeDAT.NonCopyBackWrData) and chi_txdat.bits.Data:get()[1] == 0xdead and chi_txdat.bits.BE:is(0xFF) end)

            chi_rxrsp:comp(1, 0) -- txn_id = 1, db_id = 0
            env.expect_happen_until(10, function () return chi_txrsp:fire() and chi_txrsp.bits.Opcode:is(OpcodeRSP.CompAck) end)
            env.expect_happen_until(10, function () return tl_d:fire() and tl_d.bits.sink:is(1) end)
            env.negedge()
                machines[1].io_status_state:expect(0)
            
            env.negedge(100)
        end

        -- Put and Get
        env.dut_reset()
        chi_txreq.ready:set(0)
        do
            env.negedge()
                tl_a:putfull(0x1000, 0, "0xdead")
            env.negedge()
                tl_a:get(0x1000, 0, 0xFF)
            
            chi_txreq.ready:set(1)
            env.expect_happen_until(10, function () return chi_txreq:fire() and chi_txreq.bits.Opcode:is(OpcodeREQ.WriteNoSnpPtl) and chi_txreq.bits.TxnID:is(0) end)
            env.negedge()
            env.expect_not_happen_until(10, function () return chi_txreq:fire() end)

            chi_rxrsp:dbidresp(0, 0) -- txn_id = 0, db_id = 0

            fork {
                function ()
                    env.expect_happen_until(10, function () return chi_txdat:fire() and chi_txdat.bits.Opcode:is(OpcodeDAT.NonCopyBackWrData) and chi_txdat.bits.Data:get()[1] == 0xdead and chi_txdat.bits.BE:is(0xFF) end)

                    chi_rxrsp:comp(0, 0) -- txn_id = 0, db_id = 0
                    env.expect_happen_until(10, function () return chi_txrsp:fire() and chi_txrsp.bits.Opcode:is(OpcodeRSP.CompAck) end)
                    env.expect_happen_until(10, function () return tl_d:fire() and tl_d.bits.sink:is(0) end)
                    env.negedge()
                        machines[0].io_status_state:expect(0)
                end,
                function ()
                    env.expect_happen_until(10, function () return chi_txreq:fire() and chi_txreq.bits.Opcode:is(OpcodeREQ.ReadNoSnp) and chi_txreq.bits.TxnID:is(1) end)
                    env.negedge()
                    chi_rxrsp:readreceipt(1)
                    chi_rxdat:compdat(1, "0x1122", 0, CHIResp.I) -- txn_id = 1, db_id = 0
                    env.expect_happen_until(10, function () return tl_d:fire() and tl_d.bits.opcode:is(TLMessageD.AccessAckData) and tl_d.bits.data:is_hex_str("0x1122") end)
                    env.negedge()
                        machines[1].io_status_state:expect(0)
                end
            }

            env.negedge(100)
        end
    end
}

fork {
    function ()
        -- sim.dump_wave()

        test_get()
        test_put()
        test_mix_get_put()

        env.negedge(100)
        env.TEST_SUCCESS()
    end
}