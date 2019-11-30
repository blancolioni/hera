package body Hera.Atmosphere.Configure is

   -------------------
   -- Add_Component --
   -------------------

   procedure Add_Component
     (Atm     : in out Atmosphere_Builder;
      Gas     : Atmospheric_Gas;
      Partial : Unit_Real)
   is
      Current  : Non_Negative_Real := 0.0;
      New_List : Atmospheric_Component_Lists.List;
   begin
      for Item of Atm.List loop
         if Item.Partial > 0.0 then
            New_List.Append (Item);
            Current := Current + Item.Partial;
         end if;
      end loop;

      for Item of New_List loop
         Item.Partial := Item.Partial / Current;
      end loop;

      declare
         Ratio : constant Unit_Real := 1.0 - Partial;
      begin
         for Item of New_List loop
            Item.Partial := Item.Partial * Ratio;
         end loop;
      end;

      New_List.Append ((Gas, Partial));
      Atmospheric_Sorting.Sort (New_List);
      Atm.List := New_List;
   end Add_Component;

   ---------------
   -- Normalize --
   ---------------

   procedure Normalize
     (Builder  : in out Atmosphere_Builder;
      Strategy : Normalization_Strategy)
   is
      Total : Non_Negative_Real := 0.0;
   begin
      Atmospheric_Sorting.Sort (Builder.List);
      for Item of Builder.List loop
         Total := Total + Item.Partial;
      end loop;
      if Total /= 1.0 then
         case Strategy is
            when Change_First_Gas =>
               declare
                  Item : Atmospheric_Component renames
                           Builder.List (Builder.List.First);
               begin
                  Item.Partial := Item.Partial + 1.0 - Total;
               end;
            when Change_All_Gases =>
               for Item of Builder.List loop
                  Item.Partial := Item.Partial * 1.0 / Total;
               end loop;
         end case;
      end if;
   end Normalize;

   -------------
   -- Partial --
   -------------

   function Partial
     (Atm : Atmosphere_Builder;
      Gas : Atmospheric_Gas)
      return Unit_Real
   is
   begin
      for Item of Atm.List loop
         if Item.Gas = Gas then
            return Item.Partial;
         end if;
      end loop;
      return 0.0;
   end Partial;

   ---------------------
   -- Scale_Component --
   ---------------------

   procedure Scale_Component
     (Atm   : in out Atmosphere_Builder;
      Gas   : Atmospheric_Gas;
      Scale : Non_Negative_Real)
   is
   begin
      for Item of Atm.List loop
         if Item.Gas = Gas then
            Item.Partial := Item.Partial * Scale;
            exit;
         end if;
      end loop;
   end Scale_Component;

   -------------------
   -- To_Atmosphere --
   -------------------

   function To_Atmosphere
     (Builder  : Atmosphere_Builder'Class;
      Pressure : Non_Negative_Real)
      return Atmosphere_Type
   is
   begin
      return Atm : Atmosphere_Type := Atmosphere_Type'
        (Pressure   => Pressure,
         Components => Atmospheric_Component_Lists.Empty_List)
      do
         for Item of Builder.List loop
            Atm.Components.Append (Item);
         end loop;
      end return;
   end To_Atmosphere;

end Hera.Atmosphere.Configure;
