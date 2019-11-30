with Hera.Logging;

package body Hera.Installations is

   function Get_Queue
     (Installation : Installation_Class;
      Index        : Positive)
      return Hera.Handles.Production_Queue.Production_Queue_Handle;

   ------------------
   -- Add_To_Queue --
   ------------------

   procedure Add_To_Queue
     (Installation : Installation_Class;
      Commodity    : Hera.Commodities.Commodity_Class;
      Quantity     : Hera.Quantities.Quantity_Type)
   is
      use Hera.Handles.Production_Queue;
      Index : constant Positive :=
        Installation.Queue_Last + 1;
      Queue : constant Production_Queue_Handle :=
        Get_Queue (Installation, Index);
   begin
      Installation.Update_Installation
        .Set_Queue_Start (Natural'Max (Installation.Queue_Start, 1))
        .Set_Queue_Last (Index)
        .Done;

      Queue.Update_Production_Queue
        .Set_Commodity (Commodity.Reference_Commodity)
        .Set_Quantity (Quantity)
        .Done;
      Hera.Logging.Log (Installation.Facility,
                        Hera.Quantities.Show (Quantity)
                        & " "
                        & Commodity.Tag
                        & " added to queue at position"
                        & Natural'Image
                          (Installation.Queue_Last));
   end Add_To_Queue;

   -----------------
   -- Clear_Queue --
   -----------------

   procedure Clear_Queue (Installation : Installation_Class) is
   begin
      Installation.Update_Installation
        .Set_Queue_Start (0)
        .Set_Queue_Last (0)
        .Done;
   end Clear_Queue;

   ---------------
   -- Get_Queue --
   ---------------

   function Get_Queue
     (Installation : Installation_Class;
      Index        : Positive)
      return Hera.Handles.Production_Queue.Production_Queue_Handle
   is
      use Hera.Handles.Production_Queue;
      use Hera.Handles.Production_Queue.Selections;
      Result : constant Production_Queue_Handle :=
        First_Where (Production_Queue (Installation, Index));
   begin
      if not Result.Has_Element then
         return Create
           (Has_Queue => Installation,
            Index     => Index,
            Commodity => Hera.Handles.Commodity.Empty_Handle,
            Quantity  => Hera.Quantities.Zero);
      else
         return Result;
      end if;
   end Get_Queue;

end Hera.Installations;
