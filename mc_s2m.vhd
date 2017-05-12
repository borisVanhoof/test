library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;
 
--------------------------------------------------------------------------------
-- Entity declaration
-------------------------------------------------------------------------------

entity mc_s2m is

--------------------------------------------------------------------------------
---- Entity Generic declaration
-------------------------------------------------------------------------------
  generic(
	 gen_log2MemDepth: natural:= 9; --depth of memory buffer
	 clk_delays_rvalid : natural := 1 -- the number of clock cycles between arvalid and rvalid (read delay of the BRAM)
	);
  
--------------------------------------------------------------------------------
---- Entity Port declaration
--------------------------------------------------------------------------------
  port(	
		-- Ports of Axi Slave Bus Interface S00_AXIS
		s00_axis_clock	: in std_logic;
		s00_axis_aresetn	: in std_logic;
		
		s00_axis_tready	: out std_logic;  -- oke
		s00_axis_tdata	: in std_logic_vector(31 downto 0); -- oke
		s00_axis_tstrb	: in std_logic_vector(1 downto 0); -- not used
		s00_axis_tlast	: in std_logic; -- oke
		s00_axis_tvalid	: in std_logic; -- oke
		s00_axis_tuser : in std_logic_vector(0 downto 0); -- not used

		-- Ports of Axi Slave Bus Interface S00_AXI	
		s00_axi_clock	: in std_logic;
        s00_axi_aresetn    : in std_logic;
        	
		s00_axi_awaddr	: in std_logic_vector(31 downto 0); -- oke
		s00_axi_awprot	: in std_logic_vector(2 downto 0);
		s00_axi_awvalid	: in std_logic; -- oke
		s00_axi_awready	: out std_logic; -- oke
		
		s00_axi_wdata	: in std_logic_vector(31 downto 0); -- oke
		s00_axi_wstrb	: in std_logic_vector(3 downto 0);
		s00_axi_wvalid	: in std_logic; -- oke
		s00_axi_wready	: out std_logic; -- oke
		
		s00_axi_bresp	: out std_logic_vector(1 downto 0); -- oke
		s00_axi_bvalid	: out std_logic; -- oke
		s00_axi_bready	: in std_logic;
		
		s00_axi_araddr	: in std_logic_vector(31 downto 0); -- oke
		s00_axi_arprot	: in std_logic_vector(2 downto 0);
		s00_axi_arvalid	: in std_logic; -- oke
		s00_axi_arready	: out std_logic; -- oke
		
		s00_axi_rdata	: out std_logic_vector(31 downto 0); -- oke
		s00_axi_rresp	: out std_logic_vector(1 downto 0); -- oke
		s00_axi_rvalid	: out std_logic; -- oke
		s00_axi_rready	: in std_logic
  );
  
end mc_s2m;

--------------------------------------------------------------------------------
-- Architecture declaration
-------------------------------------------------------------------------------

architecture arch of mc_s2m is

--------------------------------------------------------------------------------
---- Arch. Type declaration
-------------------------------------------------------------------------------

--------------------------------------------------------------------------------
---- Arch. Constant declaration
--------------------------------------------------------------------------------

  constant c_packetOffset : integer := 16;
--------------------------------------------------------------------------------
---- Arch. Component declaration
--------------------------------------------------------------------------------

  component simple_dual_port_ram_single_clock
  generic(
	  DATA_WIDTH : natural := 8;
	  ADDR_WIDTH : natural := 6
	);
	port(
	  --General
		clk: in std_logic;
	  --Port A
		addr_a: in std_logic_vector((ADDR_WIDTH-1) downto 0);
		data_a: in std_logic_vector((DATA_WIDTH-1) downto 0);
		we_a: in std_logic;
	  --Port B
		addr_b: in std_logic_vector((ADDR_WIDTH-1) downto 0);
		q_b: out std_logic_vector((DATA_WIDTH-1) downto 0)
	);
  end component;

--------------------------------------------------------------------------------
---- Arch. Procedure & Function declaration
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
---- Arch. Signal declaration
--------------------------------------------------------------------------------
  signal bufAddress : std_logic_vector(gen_log2MemDepth-1 downto 0);
  signal bufData    : std_logic_vector(31 downto 0);
  signal bufWrite   : std_logic;

  signal regReady  : std_logic;
  signal regEnIrq : std_logic;

  signal buf_s00_axi_rdata : std_logic_vector(31 downto 0);
  signal buf_s00_axi_araddr : std_logic_vector(gen_log2MemDepth-1 downto 0);
  
  -- this shift register is used to implement the delay between arvalid and rvalid
  signal rvalid_shift_reg : std_logic_vector(clk_delays_rvalid downto 0) := (others => '0'); 
  
  -- This is a temporary signal. In the final product this needs to be replaced with an AXI signal. Perhaps TUSER.
  signal avs_mm_irq : std_logic                     := '0';
  signal s00_axi_arready_i : std_logic := '0';
  
begin
  s00_axi_bresp <= "00";
  s00_axi_bvalid <= '1';
  s00_axi_rresp <= "00";

  -- connect one side of the buffer directly to the mm bus
  pack_buf: simple_dual_port_ram_single_clock
  generic map(
		DATA_WIDTH => 32,
		ADDR_WIDTH => gen_log2MemDepth 
	)
  port map(
	  --General
	    clk => s00_axi_clock,
	  --Port A
		addr_a => bufAddress,
		data_a => bufData,
		we_a   => bufWrite,
	  --Port B
		addr_b => s00_axi_araddr(gen_log2MemDepth+1 downto 2), -- devide by 4 (or shift 2 bits to the right)
		q_b    => buf_s00_axi_rdata
	);


  -- buffer next signal if necessary
  s00_axi_rdata <=  regEnIrq &x"0000000" & "00" & regReady when buf_s00_axi_araddr = std_logic_vector(to_unsigned(0,gen_log2MemDepth)) else buf_s00_axi_rdata;

  s00_axis_tready <= not(regReady);
  
  s00_axi_rvalid <= rvalid_shift_reg(0);

  s00_axi_arready <= s00_axi_arready_i;

  writedata : process(s00_axi_clock)
  begin
    if rising_edge(s00_axi_clock) then
      if (s00_axi_aresetn = '0') then
        avs_mm_irq <= '0';
        bufAddress <= std_logic_vector(to_unsigned(c_packetOffset - 1,gen_log2MemDepth));
        bufData <= (others => '0');
        bufWrite <= '0';
        regEnIrq <= '0';
        regReady <= '0';
	rvalid_shift_reg <= (others => '0');
	s00_axi_arready_i <= '0';
      else

        buf_s00_axi_araddr <= s00_axi_araddr(gen_log2MemDepth+1 downto 2); -- devide by 4 (or shift 2 bits to the right)
        -- only do something when downstream is ready and I'm not finished

        if ((s00_axis_tvalid = '1') and (regReady = '0')) then

          bufAddress <= std_logic_vector( unsigned(bufAddress) + 1);

          -- Data
          bufData <= s00_axis_tdata;
          bufWrite <= '1';

          -- EOP flag when address = length
          if (s00_axis_tlast = '1') then
              avs_mm_irq <= regEnIrq;
              regReady <= '1';           
          end if;
        else
          bufWrite <= '0';
        end if;

		s00_axi_awready <= s00_axi_awvalid and s00_axi_wvalid; 
		s00_axi_wready <= s00_axi_awvalid and s00_axi_wvalid; 

        if ((unsigned(s00_axi_awaddr) = 0) and (s00_axi_awvalid = '1') and (s00_axi_wvalid = '1')) then
          if ((s00_axi_wdata(0) = '0') and (regReady = '1')) then
            regReady   <= '0';
            bufAddress <= std_logic_vector(to_unsigned(c_packetOffset - 1,gen_log2MemDepth));
          end if;
          regEnIrq <= s00_axi_wdata(31);
        end if;        

		s00_axi_arready_i <= s00_axi_arvalid;
		
        if ((unsigned(s00_axi_araddr) = 0) and (s00_axi_arvalid = '1')) then
          avs_mm_irq <= '0';
        end if;
		
		-- Obviously ARVALID and RVALID cannot occure during the same clock cycle.
		-- Thats why we insert a delay of 1 clock cycle between arvalid and rvalid
		-- (maybe this should be: if(arvalid = '1' and ready = '1') then rvalid = '1' else rvalid = '0'?)
		rvalid_shift_reg(clk_delays_rvalid - 1 downto 0) <= rvalid_shift_reg(clk_delays_rvalid downto 1);
		rvalid_shift_reg(clk_delays_rvalid) <= s00_axi_arvalid and not s00_axi_arready_i;

      end if;
    end if;
  end process;

        
end arch;

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- End File
-------------------------------------------------------------------------------
