package Hera.Stars.Tables is

   procedure Get_Main_Sequence_Info
     (Solar_Masses : Non_Negative_Real;
      Radius       : out Non_Negative_Real;
      Luminosity   : out Non_Negative_Real;
      R, G, B      : out Unit_Real);

end Hera.Stars.Tables;
