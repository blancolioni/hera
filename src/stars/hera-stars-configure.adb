with Ada.Numerics;

with Hera.Calendar;
with Hera.Elementary_Functions;
with Hera.Random;

with Hera.Solar_System;

with Hera.Objects;

with Hera.Stars.Tables;

package body Hera.Stars.Configure is

   Star_Version : constant Hera.Objects.Object_Version := "0.0.1";

   function Random_Star_Mass return Non_Negative_Real;

   -------------------------------
   -- Create_Main_Sequence_Star --
   -------------------------------

   function Create_Main_Sequence_Star
     (Name   : String;
      System : Hera.Star_Systems.Star_System_Type)
      return Star_Type
   is
      Solar_Masses : constant Non_Negative_Real :=
                       Random_Star_Mass;
      Mass         : constant Non_Negative_Real :=
                       Solar_Masses * Hera.Solar_System.Solar_Mass;
      Radius       : Non_Negative_Real;
      Luminosity   : Non_Negative_Real;
      R, G, B      : Unit_Real;
   begin

      Hera.Stars.Tables.Get_Main_Sequence_Info
        (Solar_Masses => Solar_Masses,
         Radius       => Radius,
         Luminosity   => Luminosity,
         R            => R,
         G            => G,
         B            => B);

      declare
         use Hera.Elementary_Functions;
         Volume      : constant Non_Negative_Real :=
                         4.0 * Ada.Numerics.Pi * (Radius ** 3) / 3.0;
         Age         : constant Non_Negative_Real :=
                         4.0e9
                           * Solar_Masses
                         / Luminosity
                           * (Hera.Random.Unit_Random + 0.5);
         Temperature : constant Non_Negative_Real :=
                         (Luminosity ** 0.25)
                         * Hera.Solar_System.Solar_Surface_Temperature;
         Ecosphere   : constant Non_Negative_Real :=
                         Hera.Elementary_Functions.Sqrt (Luminosity);
         Star        : Root_Star_Type := Root_Star_Type'
           (Hera.Star_Systems.Root_Star_System_Entity with
            Age         => Age,
            Luminosity  => Luminosity,
            Temperature => Temperature,
            Ecosphere   => Ecosphere,
            Color       => (R, G, B, 1.0));
      begin

         Star.Initialize_Star_System_Entity
           (Name            => Name,
            System          => System,
            Version         => Star_Version,
            Mass            => Mass,
            Primary         => null,
            Orbit           => 0.0,
            Epoch           => Hera.Calendar.Start,
            Radius          => Radius,
            Density         => Mass / Volume,
            Rotation_Period => 1.0,
            Tilt            => 0.0,
            Surface_Gravity => 99.0);

         return Save_Star (Star);
      end;
   end Create_Main_Sequence_Star;

   ----------------------
   -- Random_Star_Mass --
   ----------------------

   function Random_Star_Mass return Non_Negative_Real is
      Realistic_Star_Masses : constant Boolean := False;
      Seed                  : constant Real := Hera.Random.Unit_Random;
      Solar_Mass_Count      : Real;
   begin
      if Realistic_Star_Masses then
         if Seed <= 0.99 then
            Solar_Mass_Count :=
              0.1 + 6.0 * Seed - 15.0 * Seed ** 2
                + 11.0 * Seed ** 3;
         else
            declare
               X : constant Real := (Seed - 0.99) * 1.0E4;
               A : constant Real := 0.110833;
               B : constant Real := -14.0358;
               C : constant Real := 445.25;
            begin
               Solar_Mass_Count := A * X ** 2 + B * X + C;
            end;
         end if;
      else
         Solar_Mass_Count :=
           Signed_Unit_Clamp (Hera.Random.Normal_Random (0.2))
             + 1.0;
      end if;
      return Solar_Mass_Count;
   end Random_Star_Mass;

end Hera.Stars.Configure;
