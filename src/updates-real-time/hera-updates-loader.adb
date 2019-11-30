with Hera.Markets.Updates;

with Hera.Updates.Events;

with Hera.Handles.Market;
with Hera.Db.Market;

package body Hera.Updates.Loader is

   ------------------
   -- Load_Updates --
   ------------------

   procedure Load_Updates is
   begin
      for Market of Hera.Db.Market.Scan_By_Top_Record loop
         Hera.Updates.Events.Update_At
           (Market.Next_Update,
            Hera.Markets.Updates.Daily_Update
              (Hera.Handles.Market.Get (Market.Get_Market_Reference)));
      end loop;
   end Load_Updates;

end Hera.Updates.Loader;
