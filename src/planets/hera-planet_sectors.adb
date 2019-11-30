with WL.Heaps;

with Hera.Db.Planet_Sector;

package body Hera.Planet_Sectors is

   package Planet_Sector_Heaps is
     new WL.Heaps (Non_Negative_Real, Planet_Sector_Handle, "<",
                   Hera.Handles.Planet_Sector."=");

   ----------
   -- Find --
   ----------

   function Find
     (Planet : Hera.Handles.Planet.Planet_Handle;
      Score  : not null access function
        (Planet_Sector : Planet_Sector_Handle) return Non_Negative_Real)
      return Planet_Sector_Array
   is
      Queue : Planet_Sector_Heaps.Heap;
   begin
      for Planet_Sector of
        Hera.Db.Planet_Sector.Select_By_Planet
          (Planet.Reference_Planet)
      loop
         declare
            Handle     : constant Planet_Sector_Handle :=
              Hera.Handles.Planet_Sector.Get
                (Planet_Sector.Get_Planet_Sector_Reference);
            This_Score : constant Non_Negative_Real :=
              Score (Handle);
         begin
            if This_Score > 0.0 then
               Queue.Insert (This_Score, Handle);
            end if;
         end;
      end loop;

      return Result : Planet_Sector_Array (1 .. Queue.Length) do
         for Handle of Result loop
            Handle := Queue.First_Element;
            Queue.Delete_First;
         end loop;
      end return;
   end Find;

end Hera.Planet_Sectors;
