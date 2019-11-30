package body Hera.Commodities.Maps is

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in out Map;
      Commodity : not null access constant Root_Commodity_Type'Class;
      Element   : Element_Type)
   is
   begin
      Container.Insert (Commodity.Tag, Element);
   end Insert;

end Hera.Commodities.Maps;
