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
	 gen_log2MemDepth: natural:= 9 --depth of memory buffer
	);
  
--------------------------------------------------------------------------------
---- Entity Port declaration
--------------------------------------------------------------------------------
  port(
    clk_in_clock    : in std_logic := '0';
    reset_in_reset  : in std_logic := '0';

    asi_st_data  : in std_logic_vector(31 downto 0)  := (others=>'0');
    asi_st_sop   : in std_logic                      := '0';
    asi_st_eop   : in std_logic                      := '0';
    asi_st_empty : in std_logic_vector(1 downto 0)   := (others=>'0');
    asi_st_ready : out  std_logic                      := '0';
    asi_st_error : in std_logic_vector(0 downto 0)   := (others=>'0');
    asi_st_valid : in std_logic                      := '0';

    avs_mm_address    : in  std_logic_vector(gen_log2MemDepth-1 downto 0)  := (others=>'0');
    avs_mm_irq        : out std_logic                     := '0';
    avs_mm_read       : in  std_logic                     := '0';
    avs_mm_readdata   : out std_logic_vector(31 downto 0) := (others=>'0');
    avs_mm_write      : in  std_logic                     := '0';
    avs_mm_writedata  : in  std_logic_vector(31 downto 0) := (others=>'0')
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

  signal buf_avs_mm_readdata : std_logic_vector(31 downto 0);
  signal buf_avs_mm_address : std_logic_vector(gen_log2MemDepth-1 downto 0);
  
begin

  -- connect one side of the buffer directly to the mm bus
  pack_buf: simple_dual_port_ram_single_clock
  generic map(
		DATA_WIDTH => 32,
		ADDR_WIDTH => gen_log2MemDepth 
	)
  port map(
	  --General
	  clk => clk_in_clock,
	  --Port A
		addr_a => bufAddress,
		data_a => bufData,
		we_a   => bufWrite,
	  --Port B
		addr_b => avs_mm_address,
		q_b    => buf_avs_mm_readdata
	);


  -- buffer next signal if necessary
  avs_mm_readdata <=  regEnIrq &x"0000000" & "00" & regReady when buf_avs_mm_address = std_logic_vector(to_unsigned(0,gen_log2MemDepth)) else buf_avs_mm_readdata;

  asi_st_ready <= not(regReady);

  writedata : process(clk_in_clock)
  begin
    if rising_edge(clk_in_clock) then
      if (reset_in_reset = '1') then
        avs_mm_irq <= '0';
        bufAddress <= std_logic_vector(to_unsigned(c_packetOffset - 1,gen_log2MemDepth));
        bufData <= (others => '0');
        bufWrite <= '0';
        regEnIrq <= '0';
        regReady <= '0';
      else

        buf_avs_mm_address <= avs_mm_address;
        -- only do something when downstream is ready and I'm not finished

        if ((asi_st_valid = '1') and (regReady = '0')) then

          bufAddress <= std_logic_vector( unsigned(bufAddress) + 1);

          -- Data
          bufData <= asi_st_data;
          bufWrite <= '1';

          -- EOP flag when address = length
          if (asi_st_eop = '1') then
              avs_mm_irq <= regEnIrq;
              regReady <= '1';           
          end if;
        else
          bufWrite <= '0';
        end if;

        if ((unsigned(avs_mm_address) = 0) and (avs_mm_write = '1')) then
          if ((avs_mm_writedata(0) = '0') and (regReady = '1')) then
            regReady   <= '0';
            bufAddress <= std_logic_vector(to_unsigned(c_packetOffset - 1,gen_log2MemDepth));
          end if;
          regEnIrq <= avs_mm_writedata(31);
        end if;        

        if ((unsigned(avs_mm_address) = 0) and (avs_mm_read = '1')) then
          avs_mm_irq <= '0';
        end if;

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
