with Ada.Text_IO;

with Hera.Elementary_Functions;
with Hera.Random;
with Hera.Real_Images;

with Hera.Planets.Configure;
with Hera.Stars.Configure;

package body Hera.Star_Systems.Configure is

   Log_Generation : constant Boolean := False;

   function Get_Zone
     (Star : Hera.Stars.Star_Type;
      AUs  : Non_Negative_Real)
      return Hera.Planets.Orbit_Zone;

   procedure Put (Width : Positive;
                  Value : String);

   procedure Put (Width : Positive;
                  Value : Real);

   procedure Put (Width : Positive;
                  Value : Integer);

   ------------------------
   -- Create_Star_System --
   ------------------------

   procedure Create_Star_System
     (Name    : String;
      X, Y, Z : Real)
   is
      use Hera.Random;
      System : constant Star_System_Type :=
        New_Star_System (Name, X, Y, Z);
      Star   : constant Hera.Stars.Star_Type :=
        Hera.Stars.Configure.Create_Main_Sequence_Star
          (Name   => Name,
           System => System);

      Planet_Count : constant Positive := TDR;
      Ds           : array (1 .. Planet_Count) of Non_Negative_Real;
   begin

      Set_Primary (System, Star);

      Ds (Ds'First) := D6 / 10.0;
      for I in Ds'First + 1 .. Ds'Last loop
         Ds (I) := Ds (I - 1) * (Real (D (2)) / 10.0 + 1.0);
      end loop;

      if Log_Generation then
         Put (16, Star.Name);
         Ada.Text_IO.Set_Col (24);
         Put (8, Star.Age / 1.0e9);
         Put (8, Planet_Count);

         for D of Ds loop
            declare
               use all type Hera.Planets.Orbit_Zone;
               Zone : constant Hera.Planets.Orbit_Zone :=
                 Get_Zone (Star, D);
            begin
               Ada.Text_IO.Put
                 (case Zone is
                     when Red    => 'R',
                     when Yellow => 'Y',
                     when Green  => 'G',
                     when Blue   => 'B',
                     when Black  => 'x');
            end;
         end loop;
         Ada.Text_IO.New_Line;

         Ada.Text_IO.Put ("  Planet");
         Ada.Text_IO.Set_Col (16);
         Put (8, "Zone");
         Put (12, "Type");
         Put (8, "Orbit");
         Put (8, "Mass");
         Put (8, "Density");
         Put (8, "Radius");
         Put (8, "Gravity");
         Put (8, "Year");
         Put (8, "Day");
         Put (8, "Ocean%");
         Put (8, "Temp");
         Put (16, "Life");
         Put (8, "Atm");
         Put (8, "Hab");
         Put (8, "Pressure");
         Ada.Text_IO.New_Line;
      end if;

      declare
         use all type Hera.Planets.Orbit_Zone;
         Count : Natural := 0;
      begin
         for D of Ds loop
            if Get_Zone (Star, D) > Red then
               Count := Count + 1;
               Hera.Planets.Configure.Create_Planet
                 (Star, Count, Get_Zone (Star, D), D);
            end if;
         end loop;
      end;

      if Log_Generation then
         Ada.Text_IO.New_Line;
      end if;

   end Create_Star_System;

   --------------
   -- Get_Zone --
   --------------

   function Get_Zone
     (Star : Hera.Stars.Star_Type;
      AUs  : Non_Negative_Real)
      return Hera.Planets.Orbit_Zone
   is
      use all type Hera.Planets.Orbit_Zone;
      Lum : constant Non_Negative_Real :=
              Hera.Elementary_Functions.Sqrt (Star.Luminosity);
   begin
      if AUs < Lum * 0.25 then
         return Red;
      elsif AUs < Lum * 0.75 then
         return Yellow;
      elsif AUs < Lum * 1.5 then
         return Green;
      elsif AUs < Lum * 20.0 then
         return Blue;
      else
         return Black;
      end if;
   end Get_Zone;

   ---------
   -- Put --
   ---------

   procedure Put (Width : Positive;
                  Value : String)
   is
      use Ada.Text_IO;
      Target : constant Count := Col + Count (Width);
   begin
      Put (Value);
      Set_Col (Target);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Width : Positive;
                  Value : Real)
   is
   begin
      Put (Width, Hera.Real_Images.Approximate_Image (Value));
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Width : Positive;
                  Value : Integer)
   is
   begin
      Put (Width, Value'Image);
   end Put;

end Hera.Star_Systems.Configure;
