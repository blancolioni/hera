with Hera.Sessions;

package Hera.Repl is

   procedure Read
     (Session : Hera.Sessions.Hera_Session;
      Path    : String);

   procedure Execute
     (Session : Hera.Sessions.Hera_Session);

end Hera.Repl;
