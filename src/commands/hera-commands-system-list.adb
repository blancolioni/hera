with Hera.File_System;
with Hera.Writers;

package body Hera.Commands.System.List is

   type List_Command_Record is
     new Root_Hera_Command with null record;

   overriding procedure Perform
     (Command   : List_Command_Record;
      Context   : in out Hera.Contexts.Context_Type;
      Writer    : in out Hera.Writers.Writer_Interface'Class;
      Arguments : Argument_List);

   ------------------
   -- List_Command --
   ------------------

   function List_Command return Root_Hera_Command'Class is
   begin
      return Command : List_Command_Record;
   end List_Command;

   -------------
   -- Perform --
   -------------

   overriding procedure Perform
     (Command   : List_Command_Record;
      Context   : in out Hera.Contexts.Context_Type;
      Writer    : in out Hera.Writers.Writer_Interface'Class;
      Arguments : Argument_List)
   is
      pragma Unreferenced (Command);

      Ids : Hera.Writers.Identifier_List;
      Arg_Count : constant Natural :=
        Argument_Count (Arguments);

      procedure Put_Item
        (Name  : String;
         Child : Hera.File_System.Node_Id);

      --------------
      -- Put_Item --
      --------------

      procedure Put_Item
        (Name  : String;
         Child : Hera.File_System.Node_Id)
      is
         pragma Unreferenced (Child);
      begin
         Hera.Writers.Add (Ids, Name);
      end Put_Item;

   begin

      if Arg_Count = 0 then
         declare
            Current : constant Hera.File_System.Node_Interface'Class :=
              Context.Current_Node;
         begin
            if Current.Is_Leaf then
               Writer.Put_Error
                 (". is not a directory");
            else
               Current.Iterate_Children (Put_Item'Access);
            end if;
         end;
      elsif Arg_Count = 1 then
         Context.Push_Scope;
         if Context.Change_Scope (Argument (Arguments, 1)) then
            Context.Current_Node.Iterate_Children (Put_Item'Access);
         else
            Writer.Put_Error
              ("Cannot list " & Argument (Arguments, 1));
         end if;
         Context.Pop_Scope;
      else
         Writer.Put_Error
           ("Usage: ls [scope-path]");
      end if;

      Writer.Put_Identifier_List (Ids);
   end Perform;

end Hera.Commands.System.List;
