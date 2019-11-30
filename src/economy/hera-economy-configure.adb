with Ada.Text_IO;

with Hera.Elementary_Functions;
with Hera.Random;
with Hera.Real_Images;

with Hera.Commodities.Configure;
with Hera.Markets.Configure;

with Hera.Handles.Economy;

with Hera.Db.Economic_Sector;
with Hera.Db.Economy;
with Hera.Db.Parameter;

package body Hera.Economy.Configure is

   --------------------
   -- Create_Economy --
   --------------------

   function Create_Economy
     (Economy_Name   : String;
      Economy_Config : Tropos.Configuration)
      return Hera.Db.Economy_Reference
   is

      use Hera.Elementary_Functions;

      function Get (Name : String) return Real;

      ---------
      -- Get --
      ---------

      function Get (Name : String) return Real is
         Config : constant Tropos.Configuration :=
           Economy_Config.Child (Name);
      begin
         if Config.Child_Count = 0 then
            return 0.0;
         elsif Config.Child_Count = 1 then
            return Config.Get (1);
         elsif Config.Child_Count = 2 then
            declare
               Mean : constant Real := Config.Get (1);
               Variance : constant Real := Config.Get (2);
            begin
               return Mean + Hera.Random.Normal_Random (Variance / Mean);
            end;
         else
            raise Constraint_Error with
              "expected 1 or 2 parameters for '"
              & Name & "' in economy config";
         end if;
      end Get;

      Log_Pop   : constant Real := Get ("pop");
      Gdp_Pc    : constant Real := Clamp (Get ("gdp"), 0.1, 20.0);
      Hab       : constant Real := Unit_Clamp (Get ("hab"));
      Res       : constant Real := Unit_Clamp (Get ("res"));
      Tec_Level : constant Real := Clamp (Get ("tec-level"), 0.0, 12.0);

      Economy : constant Hera.Db.Economy_Reference :=
        Hera.Db.Economy.Create
          (Name            => Economy_Name,
           Log_Pop         => Log_Pop,
           Log_Gdp         => Log (Gdp_Pc, 10.0) + Log_Pop,
           Gdp_Per_Capita  => Gdp_Pc,
           Habitability    => Hab,
           Resource_Rating => Res,
           Tec_Level       => Tec_Level);

      function Image (X : Real) return String
                      renames Hera.Real_Images.Approximate_Image;

   begin
      if False then
         Ada.Text_IO.Put (Economy_Name);
         Ada.Text_IO.Set_Col (20);
         Ada.Text_IO.Put_Line
           ("pop: " & Image (10.0 ** Log_Pop)
            & "; gdp: " & Image (Gdp_Pc * 10.0 ** Log_Pop)
            & "; per capita: " & Image (Gdp_Pc)
            & "; hab: " & Image (100.0 * Hab)
            & "; res: " & Image (100.0 * Res)
            & "; tec: " & Image (Tec_Level));
      end if;

      Hera.Markets.Configure.Initial_Market
        (Hera.Handles.Economy.Get (Economy));

      return Economy;
   end Create_Economy;

   -------------------
   -- Create_Sector --
   -------------------

   procedure Create_Sector (Sector_Config : Tropos.Configuration) is
      Plus  : constant Hera.Db.Parameter_Reference :=
        Hera.Db.Parameter.Create;
      Minus : constant Hera.Db.Parameter_Reference :=
        Hera.Db.Parameter.Create;
   begin
      Hera.Db.Economic_Sector.Create
        (Tag   => Sector_Config.Config_Name,
         Plus  => Plus,
         Minus => Minus);

      Hera.Commodities.Configure.Create_Parameters
        (Plus, Sector_Config.Child ("positive"));
      Hera.Commodities.Configure.Create_Parameters
        (Minus, Sector_Config.Child ("negative"));
   end Create_Sector;

end Hera.Economy.Configure;
