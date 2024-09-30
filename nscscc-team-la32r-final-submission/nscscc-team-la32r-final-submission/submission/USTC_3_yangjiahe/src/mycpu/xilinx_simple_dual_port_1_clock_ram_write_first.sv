// Generated by CIRCT firtool-1.62.0

    
//  Xilinx Simple Dual Port Single Clock RAM
//  This code implements a parameterizable SDP single clock memory.
//  If a reset or enable is not necessary, it may be tied off or removed from the code.

 module xilinx_simple_dual_port_1_clock_ram_write_first #(
     parameter RAM_WIDTH = 64,                       // Specify RAM data width
     parameter RAM_DEPTH = 512                      // Specify RAM depth (number of entries)
   ) (
     input [$clog2(RAM_DEPTH)-1:0] addra, // Write address bus, width determined from RAM_DEPTH
     input [$clog2(RAM_DEPTH)-1:0] addrb, // Read address bus, width determined from RAM_DEPTH
     input [RAM_WIDTH-1:0] dina,          // RAM input data
     input clka,                          // Clock
     input wea,                           // Write enable
     output [RAM_WIDTH-1:0] doutb         // RAM output data
   );
   (*ram_style="block"*)
     reg [RAM_WIDTH-1:0] BRAM [RAM_DEPTH-1:0];
     reg [$clog2(RAM_DEPTH)-1:0] addr_r;
     reg [RAM_WIDTH-1:0] ram_data = {RAM_WIDTH{1'b0}};
         
   generate
       integer ram_index;
       initial
         for (ram_index = 0; ram_index < RAM_DEPTH; ram_index = ram_index + 1)
           BRAM[ram_index] = {RAM_WIDTH{1'b0}};
   endgenerate

   always @(posedge clka) begin
       ram_data <= BRAM[addrb];
       if (wea) begin
           BRAM[addra] <= dina;
           if (addra == addrb) ram_data <= dina;
       end
   end

   assign doutb = ram_data;

   endmodule