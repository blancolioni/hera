with Hera.Db.Commodity;
with Hera.Db.Economic_Activity;
with Hera.Db.Parameter_Value;

package body Hera.Economy is

   -------------------
   -- Base_Pressure --
   -------------------

   function Base_Pressure
     (Economy   : Economy_Handle'Class;
      Commodity : Hera.Handles.Commodity.Commodity_Handle'Class)
      return Signed_Unit_Real
   is

      use all type Hera.Db.Parameter_Class;

      function Sector_Size
        (Sector : Hera.Db.Economic_Sector_Reference)
         return Unit_Real
      is (Hera.Db.Economic_Activity.Get_By_Economic_Activity
          (Economy.Reference, Sector)
          .Share);

      function Factor
        (Class : Hera.Db.Parameter_Class;
         Sector : Hera.Db.Economic_Sector_Reference)
         return Real
      is (case Class is
             when Habitability    =>
                2.0 * Economy.Habitability - 1.0,
             when Resource_Rating =>
                2.0 * Economy.Resource_Rating - 1.0,
             when Population      =>
                Economy.Log_Pop / 4.0 - 1.0,
             when Gdp_Per_Capita  =>
                Economy.Gdp_Per_Capita / 6.0 - 1.0,
             when Tec_Level       =>
                Economy.Tec_Level / 6.0 - 1.0,
             when Hera.Db.Sector  =>
                Sector_Size (Sector) * 5.0 - 1.0,
             when System_Size     =>
                0.0,
             when Factor          =>
                1.0);

      P_Demand : constant Hera.Db.Parameter_Reference :=
        Hera.Db.Commodity.Get (Commodity.Reference).Demand;
      P_Supply : constant Hera.Db.Parameter_Reference :=
        Hera.Db.Commodity.Get (Commodity.Reference).Supply;

      Parameter_Count : Natural := 0;
      Result          : Real := 0.0;

   begin
      for Value of
        Hera.Db.Parameter_Value.Select_By_Parameter (P_Demand)
      loop
         declare
            Class : constant Hera.Db.Parameter_Class :=
              Value.Class;
            F     : constant Real := Factor (Class, Value.Economic_Sector);
         begin
            Result := Result + F * Value.Value;
            Parameter_Count := Parameter_Count + 1;
         end;
      end loop;

      for Value of
        Hera.Db.Parameter_Value.Select_By_Parameter (P_Supply)
      loop
         declare
            Class : constant Hera.Db.Parameter_Class :=
              Value.Class;
            F     : constant Real := Factor (Class, Value.Economic_Sector);
         begin
            Result := Result - F * Value.Value;
            Parameter_Count := Parameter_Count + 1;
         end;
      end loop;

      if Parameter_Count > 0 then
         Result := Result / Real (Parameter_Count);
      end if;

      if Economy.Tec_Level < Commodity.Consume_Tec then
         Result := -1.0;
      elsif Economy.Tec_Level < Commodity.Produce_Tec then
         Result := Result + 1.0 -
           (Economy.Tec_Level - Commodity.Consume_Tec)
           / (Commodity.Produce_Tec - Commodity.Consume_Tec);
      end if;

      return Signed_Unit_Clamp (Result);

   end Base_Pressure;

end Hera.Economy;
