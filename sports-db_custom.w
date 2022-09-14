&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          sports           PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define LAYOUT-VARIABLE wWin-layout

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Customer

/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain Customer.Cust-Num Customer.Name ~
Customer.Contact Customer.Phone 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain Customer.Cust-Num ~
Customer.Name Customer.Contact Customer.Phone 
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain Customer
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain Customer
&Scoped-define QUERY-STRING-fMain FOR EACH Customer EXCLUSIVE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH Customer EXCLUSIVE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain Customer
&Scoped-define FIRST-TABLE-IN-QUERY-fMain Customer


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Customer.Cust-Num Customer.Name ~
Customer.Contact Customer.Phone 
&Scoped-define ENABLED-TABLES Customer
&Scoped-define FIRST-ENABLED-TABLE Customer
&Scoped-Define ENABLED-OBJECTS RECT-8 RECT-9 btn-6 btn-7 btn-5 btn-1 btn-2 ~
btn-3 btn-4 fll-1 btn-8 
&Scoped-Define DISPLAYED-FIELDS Customer.Cust-Num Customer.Name ~
Customer.Contact Customer.Phone 
&Scoped-define DISPLAYED-TABLES Customer
&Scoped-define FIRST-DISPLAYED-TABLE Customer
&Scoped-Define DISPLAYED-OBJECTS fll-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* Define a variable to store the name of the active layout.            */
DEFINE VAR wWin-layout AS CHAR INITIAL "Master Layout":U NO-UNDO.

/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-fMain TITLE "Options"
       MENU-ITEM m_New          LABEL "&Create New"   
       MENU-ITEM m_Delete       LABEL "&Delete"       
       MENU-ITEM m_Save         LABEL "&Save"         
       RULE
       MENU-ITEM m_Find_First   LABEL "Find &First"   
       MENU-ITEM m_Find_Previous LABEL "Find &Previous"
       MENU-ITEM m_Find_Next    LABEL "Find &Next"    
       MENU-ITEM m_Find_Last    LABEL "Find &Last"    .


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-1 
     LABEL "<<" 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 11 BY 1
     &ELSE SIZE 11 BY .95 &ENDIF.

DEFINE BUTTON btn-2 
     LABEL "<" 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 11 BY 1
     &ELSE SIZE 11 BY .95 &ENDIF.

DEFINE BUTTON btn-3 
     LABEL ">" 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 11 BY 1
     &ELSE SIZE 11 BY .95 &ENDIF.

DEFINE BUTTON btn-4 
     LABEL ">>" 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 11 BY 1
     &ELSE SIZE 11 BY .95 &ENDIF.

DEFINE BUTTON btn-5 
     LABEL "&Save" 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 11 BY 1
     &ELSE SIZE 11 BY .95 &ENDIF.

DEFINE BUTTON btn-6 
     LABEL "&New" 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 11 BY 1
     &ELSE SIZE 11 BY .95 &ENDIF.

DEFINE BUTTON btn-7 
     LABEL "&Delete" 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 11 BY 1
     &ELSE SIZE 11 BY .95 &ENDIF.

DEFINE BUTTON btn-8 
     LABEL "&OK" 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 11 BY 1
     &ELSE SIZE 11 BY .95 &ENDIF.

DEFINE VARIABLE fll-1 AS INTEGER FORMAT ">>,>>9":U INITIAL 0 
     LABEL "Find" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 24 BY 1
     &ELSE SIZE 24 BY .95 &ENDIF NO-UNDO.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 50 BY 2
     &ELSE SIZE 51.4 BY .1 &ENDIF.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 50 BY 2
     &ELSE SIZE 51.4 BY .1 &ENDIF.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      Customer SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     btn-6
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 9
          &ELSE AT ROW 1.24 COL 9 &ENDIF WIDGET-ID 30
     btn-7
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 21
          &ELSE AT ROW 1.24 COL 21 &ENDIF WIDGET-ID 32
     btn-5
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 33
          &ELSE AT ROW 1.24 COL 33 &ENDIF WIDGET-ID 28
     btn-1
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 3
          &ELSE AT ROW 2.43 COL 3 &ENDIF WIDGET-ID 20
     btn-2
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 15
          &ELSE AT ROW 2.43 COL 15 &ENDIF WIDGET-ID 22
     btn-3
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 27
          &ELSE AT ROW 2.43 COL 27 &ENDIF WIDGET-ID 24
     btn-4
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 39
          &ELSE AT ROW 2.43 COL 39 &ENDIF WIDGET-ID 26
     fll-1
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 12 COLON-ALIGNED
          &ELSE AT ROW 3.86 COL 12 COLON-ALIGNED &ENDIF WIDGET-ID 46
     btn-8
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 39
          &ELSE AT ROW 3.86 COL 39 &ENDIF WIDGET-ID 48
     Customer.Cust-Num
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 12 COLON-ALIGNED
          &ELSE AT ROW 5.29 COL 12 COLON-ALIGNED &ENDIF WIDGET-ID 14
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 12 BY 1
          &ELSE SIZE 12 BY .95 &ENDIF
     Customer.Name
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 12 COLON-ALIGNED
          &ELSE AT ROW 6.24 COL 12 COLON-ALIGNED &ENDIF WIDGET-ID 16
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 36 BY 1
          &ELSE SIZE 36 BY .95 &ENDIF
     Customer.Contact
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 7 COL 12 COLON-ALIGNED
          &ELSE AT ROW 7.19 COL 12 COLON-ALIGNED &ENDIF WIDGET-ID 12
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 36 BY 1
          &ELSE SIZE 36 BY .95 &ENDIF
     Customer.Phone
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 12 COLON-ALIGNED
          &ELSE AT ROW 8.14 COL 12 COLON-ALIGNED &ENDIF WIDGET-ID 18
          VIEW-AS FILL-IN 
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 36 BY 1
          &ELSE SIZE 36 BY .95 &ENDIF
     "sports-db_custom.w [v1.02]" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 27 BY 1
          &ELSE SIZE 27 BY .95 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 9 COL 2
          &ELSE AT ROW 9.33 COL 2 &ENDIF WIDGET-ID 40
     RECT-8
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 1
          &ELSE AT ROW 3.62 COL 1 &ENDIF WIDGET-ID 44
     RECT-9
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 1
          &ELSE AT ROW 5.05 COL 1 &ENDIF WIDGET-ID 50
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS THREE-D 
         AT COL 1 ROW 1
         SIZE 51.8 BY 9.52 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Sports DB - Customers"
         HEIGHT             = 9.52
         WIDTH              = 51.8
         MAX-HEIGHT         = 45.86
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 45.86
         VIRTUAL-WIDTH      = 256
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT wWin:LOAD-ICON("trans%.ico":U) THEN
    MESSAGE "Unable to load icon: trans%.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME UNDERLINE                                                 */
ASSIGN 
       FRAME fMain:POPUP-MENU       = MENU POPUP-MENU-fMain:HANDLE.


/* _MULTI-LAYOUT-RUN-TIME-ADJUSTMENTS */

/* LAYOUT-NAME: "Standard Character"
   LAYOUT-TYPE: CHARACTER
   EXPRESSION:  SESSION:DISPLAY-TYPE = 'TTY':U 
   COMMENT:     This layout is the standard layout specification for
                 a customized Character based terminal.  It is usually
                 selected to modify a window that has a GUI based
                 master layout.
                                                                        */
IF SESSION:DISPLAY-TYPE = 'TTY':U  THEN 
  DYNAMIC-FUNCTION('setDefaultLayout':U, 'Standard Character':U) NO-ERROR.

/* LAYOUT-NAME: "Standard MS Windows"
   LAYOUT-TYPE: GUI
   EXPRESSION:  SESSION:WINDOW-SYSTEM = 'MS-WINDOWS':U 
   COMMENT:     This layout is the standard layout specification for
                 a customized MS Windows window.  It is usually
                 selected to modify a window that needs to have a
                 standard "MS Windows" look.
                                                                        */
ELSE IF SESSION:WINDOW-SYSTEM = 'MS-WINDOWS':U  THEN 
  DYNAMIC-FUNCTION('setDefaultLayout':U, 'Standard MS Windows':U) NO-ERROR.

/* LAYOUT-NAME: "Standard Windows 95"
   LAYOUT-TYPE: CHARACTER
   EXPRESSION:  SESSION:WINDOW-SYSTEM = 'MS-WIN95':U
   COMMENT:     This layout is the standard layout specification for
                 a customized Windows 95 window.  It is usually
                 selected to modify a window that needs to have a
                 standard "Windows 95" look.
                                                                        */
ELSE IF SESSION:WINDOW-SYSTEM = 'MS-WIN95':U THEN 
  DYNAMIC-FUNCTION('setDefaultLayout':U, 'Standard Windows 95':U) NO-ERROR.

DYNAMIC-FUNCTION('setLayoutOptions':U, "Master Layout,Standard Character,Standard MS Windows,Standard Windows 95":U).

/* END-OF-LAYOUT-DEFINITIONS */

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "sports.Customer"
     _Options          = "EXCLUSIVE-LOCK"
     _Query            is OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Sports DB - Customers */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Sports DB - Customers */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-1 wWin
ON CHOOSE OF btn-1 IN FRAME fMain /* << */
DO:
    FIND FIRST customer EXCLUSIVE-LOCK NO-ERROR.
    RUN RecShow.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-2 wWin
ON CHOOSE OF btn-2 IN FRAME fMain /* < */
DO:
    FIND PREV customer EXCLUSIVE-LOCK NO-ERROR.
    RUN RecShow.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-3 wWin
ON CHOOSE OF btn-3 IN FRAME fMain /* > */
DO:
    FIND NEXT customer EXCLUSIVE-LOCK NO-ERROR.
    RUN RecShow.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-4 wWin
ON CHOOSE OF btn-4 IN FRAME fMain /* >> */
DO:
    FIND LAST customer EXCLUSIVE-LOCK NO-ERROR.
    RUN RecShow.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-5 wWin
ON CHOOSE OF btn-5 IN FRAME fMain /* Save */
DO:
    RUN RecSave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-6 wWin
ON CHOOSE OF btn-6 IN FRAME fMain /* New */
DO:
    RUN RecNew.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-7 wWin
ON CHOOSE OF btn-7 IN FRAME fMain /* Delete */
DO:
    RUN RecDelete.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-8 wWin
ON CHOOSE OF btn-8 IN FRAME fMain /* OK */
DO:
    DO WITH FRAME {&FRAME-NAME}:
        FIND customer WHERE customer.cust-num = INT(fll-1:SCREEN-VALUE) 
            EXCLUSIVE-LOCK NO-ERROR.
        fll-1:SCREEN-VALUE = "".
        RUN RecShow.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Delete wWin
ON CHOOSE OF MENU-ITEM m_Delete /* Delete */
DO:
    RUN RecDelete.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Find_First
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Find_First wWin
ON CHOOSE OF MENU-ITEM m_Find_First /* Find First */
DO:
    FIND FIRST customer EXCLUSIVE-LOCK NO-ERROR.
    RUN RecShow.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Find_Last
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Find_Last wWin
ON CHOOSE OF MENU-ITEM m_Find_Last /* Find Last */
DO:
    FIND LAST customer EXCLUSIVE-LOCK NO-ERROR.
    RUN RecShow.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Find_Next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Find_Next wWin
ON CHOOSE OF MENU-ITEM m_Find_Next /* Find Next */
DO:
    FIND NEXT customer EXCLUSIVE-LOCK NO-ERROR.
    RUN RecShow.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Find_Previous
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Find_Previous wWin
ON CHOOSE OF MENU-ITEM m_Find_Previous /* Find Previous */
DO:
    FIND PREV customer EXCLUSIVE-LOCK NO-ERROR.
    RUN RecShow.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_New
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_New wWin
ON CHOOSE OF MENU-ITEM m_New /* Create New */
DO:
    RUN RecNew.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Save wWin
ON CHOOSE OF MENU-ITEM m_Save /* Save */
DO:
    RUN RecSave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

FIND FIRST customer EXCLUSIVE-LOCK NO-ERROR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/

  {&OPEN-QUERY-fMain}
  GET FIRST fMain.
  DISPLAY fll-1 
      WITH FRAME fMain IN WINDOW wWin.
  IF AVAILABLE Customer THEN 
    DISPLAY Customer.Cust-Num Customer.Name Customer.Contact Customer.Phone 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-8 RECT-9 btn-6 btn-7 btn-5 btn-1 btn-2 btn-3 btn-4 fll-1 btn-8 
         Customer.Cust-Num Customer.Name Customer.Contact Customer.Phone 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RecDelete wWin 
PROCEDURE RecDelete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME {&FRAME-NAME}:

    MESSAGE "Do you want to delete the register?" SKIP "This action can't be undone."
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
    TITLE "Delete registry" UPDATE lChoice AS LOGICAL.

    CASE lChoice:
        WHEN TRUE THEN DO:
            DELETE customer.
    
        ASSIGN customer.cust-num:SCREEN-VALUE = ""
            customer.NAME:SCREEN-VALUE = ""
            customer.contact:SCREEN-VALUE = ""
            customer.phone:SCREEN-VALUE = "".
        END.
        WHEN FALSE THEN DO:
            MESSAGE "Operation skipped."
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
                TITLE "Operation skipped".
            RETURN NO-APPLY.
        END. 
            
    END.
END.

RUN RecShow.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RecNew wWin 
PROCEDURE RecNew :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

CREATE customer.
    ASSIGN NAME = "New User".

    RUN RecShow.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RecSave wWin 
PROCEDURE RecSave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME {&FRAME-NAME}:
    IF AVAILABLE customer THEN DO:
            ASSIGN customer.cust-num
                    customer.NAME
                    customer.contact
                    customer.phone.
    END.

    MESSAGE "Saved Succesfully!"
        VIEW-AS ALERT-BOX INFORMATION
        TITLE "Saved".

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RecShow wWin 
PROCEDURE RecShow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF AVAILABLE customer THEN DO:
    DISPLAY customer.cust-num
            customer.NAME
            customer.contact
            customer.phone
            WITH FRAME {&FRAME-NAME}.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK  _PROCEDURE wWin-layouts _LAYOUT-CASES
PROCEDURE wWin-layouts:
  DEFINE INPUT PARAMETER layout AS CHARACTER                     NO-UNDO.
  DEFINE VARIABLE lbl-hndl AS WIDGET-HANDLE                      NO-UNDO.
  DEFINE VARIABLE widg-pos AS DECIMAL                            NO-UNDO.

  /* Copy the name of the active layout into a variable accessible to   */
  /* the rest of this file.                                             */
  wWin-layout = layout.

  CASE layout:
    WHEN "Master Layout" THEN DO:
      ASSIGN
         &IF '{&WINDOW-SYSTEM}' NE 'TTY':U &THEN
         wWin:HIDDEN                                       = yes &ENDIF
         &IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
         wWin:HEIGHT                                       = 9.52 &ENDIF
         &IF '{&WINDOW-SYSTEM}' NE 'TTY':U &THEN
         wWin:WIDTH                                        = 51.8 &ENDIF.

      ASSIGN
         &IF '{&WINDOW-SYSTEM}' NE 'TTY':U &THEN
         FRAME fMain:HIDDEN                                = yes &ENDIF
         FRAME fMain:HEIGHT                                = 9.52
         FRAME fMain:WIDTH                                 = 51.8.

      ASSIGN
         btn-1:HIDDEN IN FRAME fMain                       = yes
         btn-1:HEIGHT IN FRAME fMain                       = .95
         btn-1:ROW IN FRAME fMain                          = 2.43
         btn-1:HIDDEN IN FRAME fMain                       = no.

      ASSIGN
         btn-2:HIDDEN IN FRAME fMain                       = yes
         btn-2:HEIGHT IN FRAME fMain                       = .95
         btn-2:ROW IN FRAME fMain                          = 2.43
         btn-2:HIDDEN IN FRAME fMain                       = no.

      ASSIGN
         btn-3:HIDDEN IN FRAME fMain                       = yes
         btn-3:HEIGHT IN FRAME fMain                       = .95
         btn-3:ROW IN FRAME fMain                          = 2.43
         btn-3:HIDDEN IN FRAME fMain                       = no.

      ASSIGN
         btn-4:HIDDEN IN FRAME fMain                       = yes
         btn-4:HEIGHT IN FRAME fMain                       = .95
         btn-4:ROW IN FRAME fMain                          = 2.43
         btn-4:HIDDEN IN FRAME fMain                       = no.

      ASSIGN
         btn-5:HIDDEN IN FRAME fMain                       = yes
         btn-5:HEIGHT IN FRAME fMain                       = .95
         btn-5:ROW IN FRAME fMain                          = 1.24
         btn-5:HIDDEN IN FRAME fMain                       = no.

      ASSIGN
         btn-6:HIDDEN IN FRAME fMain                       = yes
         btn-6:HEIGHT IN FRAME fMain                       = .95
         btn-6:ROW IN FRAME fMain                          = 1.24
         btn-6:HIDDEN IN FRAME fMain                       = no.

      ASSIGN
         btn-7:HIDDEN IN FRAME fMain                       = yes
         btn-7:HEIGHT IN FRAME fMain                       = .95
         btn-7:ROW IN FRAME fMain                          = 1.24
         btn-7:HIDDEN IN FRAME fMain                       = no.

      ASSIGN
         btn-8:HIDDEN IN FRAME fMain                       = yes
         btn-8:HEIGHT IN FRAME fMain                       = .95
         btn-8:ROW IN FRAME fMain                          = 3.86
         btn-8:HIDDEN IN FRAME fMain                       = no.

      ASSIGN
         Customer.Contact:HIDDEN IN FRAME fMain            = yes
         Customer.Contact:HEIGHT IN FRAME fMain            = .95
         widg-pos = Customer.Contact:ROW IN FRAME fMain 
         Customer.Contact:ROW IN FRAME fMain               = 7.19
         lbl-hndl = Customer.Contact:SIDE-LABEL-HANDLE IN FRAME fMain 
         lbl-hndl:ROW = lbl-hndl:ROW + Customer.Contact:ROW IN FRAME fMain  - widg-pos
         Customer.Contact:HIDDEN IN FRAME fMain            = no.

      ASSIGN
         Customer.Cust-Num:HIDDEN IN FRAME fMain           = yes
         Customer.Cust-Num:HEIGHT IN FRAME fMain           = .95
         widg-pos = Customer.Cust-Num:ROW IN FRAME fMain 
         Customer.Cust-Num:ROW IN FRAME fMain              = 5.29
         lbl-hndl = Customer.Cust-Num:SIDE-LABEL-HANDLE IN FRAME fMain 
         lbl-hndl:ROW = lbl-hndl:ROW + Customer.Cust-Num:ROW IN FRAME fMain  - widg-pos
         Customer.Cust-Num:HIDDEN IN FRAME fMain           = no.

      ASSIGN
         fll-1:HIDDEN IN FRAME fMain                       = yes
         fll-1:HEIGHT IN FRAME fMain                       = .95
         widg-pos = fll-1:ROW IN FRAME fMain 
         fll-1:ROW IN FRAME fMain                          = 3.86
         lbl-hndl = fll-1:SIDE-LABEL-HANDLE IN FRAME fMain 
         lbl-hndl:ROW = lbl-hndl:ROW + fll-1:ROW IN FRAME fMain  - widg-pos
         fll-1:HIDDEN IN FRAME fMain                       = no.

      ASSIGN
         Customer.Name:HIDDEN IN FRAME fMain               = yes
         Customer.Name:HEIGHT IN FRAME fMain               = .95
         widg-pos = Customer.Name:ROW IN FRAME fMain 
         Customer.Name:ROW IN FRAME fMain                  = 6.24
         lbl-hndl = Customer.Name:SIDE-LABEL-HANDLE IN FRAME fMain 
         lbl-hndl:ROW = lbl-hndl:ROW + Customer.Name:ROW IN FRAME fMain  - widg-pos
         Customer.Name:HIDDEN IN FRAME fMain               = no.

      ASSIGN
         Customer.Phone:HIDDEN IN FRAME fMain              = yes
         Customer.Phone:HEIGHT IN FRAME fMain              = .95
         widg-pos = Customer.Phone:ROW IN FRAME fMain 
         Customer.Phone:ROW IN FRAME fMain                 = 8.14
         lbl-hndl = Customer.Phone:SIDE-LABEL-HANDLE IN FRAME fMain 
         lbl-hndl:ROW = lbl-hndl:ROW + Customer.Phone:ROW IN FRAME fMain  - widg-pos
         Customer.Phone:HIDDEN IN FRAME fMain              = no.

      ASSIGN
         RECT-8:HIDDEN IN FRAME fMain                      = yes
         RECT-8:HEIGHT IN FRAME fMain                      = .1
         RECT-8:ROW IN FRAME fMain                         = 3.62
         RECT-8:WIDTH IN FRAME fMain                       = 51.4
         RECT-8:HIDDEN IN FRAME fMain                      = no.

      ASSIGN
         RECT-9:HIDDEN IN FRAME fMain                      = yes
         RECT-9:HEIGHT IN FRAME fMain                      = .1
         RECT-9:ROW IN FRAME fMain                         = 5.05
         RECT-9:WIDTH IN FRAME fMain                       = 51.4
         RECT-9:HIDDEN IN FRAME fMain                      = no.

      ASSIGN

         FRAME fMain:VIRTUAL-HEIGHT                        = 9.52
                    WHEN FRAME fMain:SCROLLABLE

         FRAME fMain:VIRTUAL-WIDTH                         = 51.80
                    WHEN FRAME fMain:SCROLLABLE
         &IF '{&WINDOW-SYSTEM}' NE 'TTY':U &THEN
         FRAME fMain:HIDDEN                                = no &ENDIF.

      ASSIGN
         &IF '{&WINDOW-SYSTEM}' NE 'TTY':U &THEN
         wWin:HIDDEN                                       = no &ENDIF.

    END.  /* Master Layout Layout Case */

    WHEN "Standard Character":U THEN DO:
      ASSIGN
         &IF '{&WINDOW-SYSTEM}' NE 'TTY':U &THEN
         FRAME fMain:HIDDEN                                = yes &ENDIF
         FRAME fMain:HEIGHT                                = 10
         FRAME fMain:WIDTH                                 = 80 NO-ERROR.

      ASSIGN
         btn-1:HIDDEN IN FRAME fMain                       = yes
         btn-1:HEIGHT IN FRAME fMain                       = 1
         btn-1:ROW IN FRAME fMain                          = 2
         btn-1:HIDDEN IN FRAME fMain                       = no NO-ERROR.

      ASSIGN
         btn-2:HIDDEN IN FRAME fMain                       = yes
         btn-2:HEIGHT IN FRAME fMain                       = 1
         btn-2:ROW IN FRAME fMain                          = 2
         btn-2:HIDDEN IN FRAME fMain                       = no NO-ERROR.

      ASSIGN
         btn-3:HIDDEN IN FRAME fMain                       = yes
         btn-3:HEIGHT IN FRAME fMain                       = 1
         btn-3:ROW IN FRAME fMain                          = 2
         btn-3:HIDDEN IN FRAME fMain                       = no NO-ERROR.

      ASSIGN
         btn-4:HIDDEN IN FRAME fMain                       = yes
         btn-4:HEIGHT IN FRAME fMain                       = 1
         btn-4:ROW IN FRAME fMain                          = 2
         btn-4:HIDDEN IN FRAME fMain                       = no NO-ERROR.

      ASSIGN
         btn-5:HIDDEN IN FRAME fMain                       = yes
         btn-5:HEIGHT IN FRAME fMain                       = 1
         btn-5:ROW IN FRAME fMain                          = 1
         btn-5:HIDDEN IN FRAME fMain                       = no NO-ERROR.

      ASSIGN
         btn-6:HIDDEN IN FRAME fMain                       = yes
         btn-6:HEIGHT IN FRAME fMain                       = 1
         btn-6:ROW IN FRAME fMain                          = 1
         btn-6:HIDDEN IN FRAME fMain                       = no NO-ERROR.

      ASSIGN
         btn-7:HIDDEN IN FRAME fMain                       = yes
         btn-7:HEIGHT IN FRAME fMain                       = 1
         btn-7:ROW IN FRAME fMain                          = 1
         btn-7:HIDDEN IN FRAME fMain                       = no NO-ERROR.

      ASSIGN
         btn-8:HIDDEN IN FRAME fMain                       = yes
         btn-8:HEIGHT IN FRAME fMain                       = 1
         btn-8:ROW IN FRAME fMain                          = 4
         btn-8:HIDDEN IN FRAME fMain                       = no NO-ERROR.

      ASSIGN
         Customer.Contact:HIDDEN IN FRAME fMain            = yes
         Customer.Contact:HEIGHT IN FRAME fMain            = 1
         widg-pos = Customer.Contact:ROW IN FRAME fMain 
         Customer.Contact:ROW IN FRAME fMain               = 7
         lbl-hndl = Customer.Contact:SIDE-LABEL-HANDLE IN FRAME fMain 
         lbl-hndl:ROW = lbl-hndl:ROW + Customer.Contact:ROW IN FRAME fMain  - widg-pos
         Customer.Contact:HIDDEN IN FRAME fMain            = no NO-ERROR.

      ASSIGN
         Customer.Cust-Num:HIDDEN IN FRAME fMain           = yes
         Customer.Cust-Num:HEIGHT IN FRAME fMain           = 1
         widg-pos = Customer.Cust-Num:ROW IN FRAME fMain 
         Customer.Cust-Num:ROW IN FRAME fMain              = 5
         lbl-hndl = Customer.Cust-Num:SIDE-LABEL-HANDLE IN FRAME fMain 
         lbl-hndl:ROW = lbl-hndl:ROW + Customer.Cust-Num:ROW IN FRAME fMain  - widg-pos
         Customer.Cust-Num:HIDDEN IN FRAME fMain           = no NO-ERROR.

      ASSIGN
         fll-1:HIDDEN IN FRAME fMain                       = yes
         fll-1:HEIGHT IN FRAME fMain                       = 1
         widg-pos = fll-1:ROW IN FRAME fMain 
         fll-1:ROW IN FRAME fMain                          = 4
         lbl-hndl = fll-1:SIDE-LABEL-HANDLE IN FRAME fMain 
         lbl-hndl:ROW = lbl-hndl:ROW + fll-1:ROW IN FRAME fMain  - widg-pos
         fll-1:HIDDEN IN FRAME fMain                       = no NO-ERROR.

      ASSIGN
         Customer.Name:HIDDEN IN FRAME fMain               = yes
         Customer.Name:HEIGHT IN FRAME fMain               = 1
         widg-pos = Customer.Name:ROW IN FRAME fMain 
         Customer.Name:ROW IN FRAME fMain                  = 6
         lbl-hndl = Customer.Name:SIDE-LABEL-HANDLE IN FRAME fMain 
         lbl-hndl:ROW = lbl-hndl:ROW + Customer.Name:ROW IN FRAME fMain  - widg-pos
         Customer.Name:HIDDEN IN FRAME fMain               = no NO-ERROR.

      ASSIGN
         Customer.Phone:HIDDEN IN FRAME fMain              = yes
         Customer.Phone:HEIGHT IN FRAME fMain              = 1
         widg-pos = Customer.Phone:ROW IN FRAME fMain 
         Customer.Phone:ROW IN FRAME fMain                 = 8
         lbl-hndl = Customer.Phone:SIDE-LABEL-HANDLE IN FRAME fMain 
         lbl-hndl:ROW = lbl-hndl:ROW + Customer.Phone:ROW IN FRAME fMain  - widg-pos
         Customer.Phone:HIDDEN IN FRAME fMain              = no NO-ERROR.

      ASSIGN
         RECT-8:HIDDEN IN FRAME fMain                      = yes
         RECT-8:HEIGHT IN FRAME fMain                      = 2
         RECT-8:ROW IN FRAME fMain                         = 4
         RECT-8:WIDTH IN FRAME fMain                       = 50
         RECT-8:HIDDEN IN FRAME fMain                      = no NO-ERROR.

      ASSIGN
         RECT-9:HIDDEN IN FRAME fMain                      = yes
         RECT-9:HEIGHT IN FRAME fMain                      = 2
         RECT-9:ROW IN FRAME fMain                         = 5
         RECT-9:WIDTH IN FRAME fMain                       = 50
         RECT-9:HIDDEN IN FRAME fMain                      = no NO-ERROR.

      ASSIGN

         FRAME fMain:VIRTUAL-HEIGHT                        = 10.00
                    WHEN FRAME fMain:SCROLLABLE

         FRAME fMain:VIRTUAL-WIDTH                         = 80.00
                    WHEN FRAME fMain:SCROLLABLE
         &IF '{&WINDOW-SYSTEM}' NE 'TTY':U &THEN
         FRAME fMain:HIDDEN                                = no &ENDIF NO-ERROR.

    END.  /* Standard Character Layout Case */

    WHEN "Standard MS Windows":U THEN DO:
      ASSIGN
         &IF '{&WINDOW-SYSTEM}' NE 'TTY':U &THEN
         wWin:HIDDEN                                       = yes &ENDIF
         &IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
         wWin:HEIGHT                                       = 9.48 &ENDIF.

      ASSIGN
         btn-1:HIDDEN IN FRAME fMain                       = yes
         btn-1:HEIGHT IN FRAME fMain                       = 1
         btn-1:ROW IN FRAME fMain                          = 2
         btn-1:HIDDEN IN FRAME fMain                       = no.

      ASSIGN
         btn-2:HIDDEN IN FRAME fMain                       = yes
         btn-2:HEIGHT IN FRAME fMain                       = 1
         btn-2:ROW IN FRAME fMain                          = 2
         btn-2:HIDDEN IN FRAME fMain                       = no.

      ASSIGN
         btn-3:HIDDEN IN FRAME fMain                       = yes
         btn-3:HEIGHT IN FRAME fMain                       = 1
         btn-3:ROW IN FRAME fMain                          = 2
         btn-3:HIDDEN IN FRAME fMain                       = no.

      ASSIGN
         btn-4:HIDDEN IN FRAME fMain                       = yes
         btn-4:HEIGHT IN FRAME fMain                       = 1
         btn-4:ROW IN FRAME fMain                          = 2
         btn-4:HIDDEN IN FRAME fMain                       = no.

      ASSIGN
         btn-5:HIDDEN IN FRAME fMain                       = yes
         btn-5:HEIGHT IN FRAME fMain                       = 1
         btn-5:ROW IN FRAME fMain                          = 1
         btn-5:HIDDEN IN FRAME fMain                       = no.

      ASSIGN
         btn-6:HIDDEN IN FRAME fMain                       = yes
         btn-6:HEIGHT IN FRAME fMain                       = 1
         btn-6:ROW IN FRAME fMain                          = 1
         btn-6:HIDDEN IN FRAME fMain                       = no.

      ASSIGN
         btn-7:HIDDEN IN FRAME fMain                       = yes
         btn-7:HEIGHT IN FRAME fMain                       = 1
         btn-7:ROW IN FRAME fMain                          = 1
         btn-7:HIDDEN IN FRAME fMain                       = no.

      ASSIGN
         btn-8:HIDDEN IN FRAME fMain                       = yes
         btn-8:HEIGHT IN FRAME fMain                       = 1
         btn-8:ROW IN FRAME fMain                          = 4
         btn-8:HIDDEN IN FRAME fMain                       = no.

      ASSIGN
         Customer.Contact:HIDDEN IN FRAME fMain            = yes
         Customer.Contact:HEIGHT IN FRAME fMain            = 1
         widg-pos = Customer.Contact:ROW IN FRAME fMain 
         Customer.Contact:ROW IN FRAME fMain               = 7
         lbl-hndl = Customer.Contact:SIDE-LABEL-HANDLE IN FRAME fMain 
         lbl-hndl:ROW = lbl-hndl:ROW + Customer.Contact:ROW IN FRAME fMain  - widg-pos
         Customer.Contact:HIDDEN IN FRAME fMain            = no.

      ASSIGN
         Customer.Cust-Num:HIDDEN IN FRAME fMain           = yes
         Customer.Cust-Num:HEIGHT IN FRAME fMain           = 1
         widg-pos = Customer.Cust-Num:ROW IN FRAME fMain 
         Customer.Cust-Num:ROW IN FRAME fMain              = 5
         lbl-hndl = Customer.Cust-Num:SIDE-LABEL-HANDLE IN FRAME fMain 
         lbl-hndl:ROW = lbl-hndl:ROW + Customer.Cust-Num:ROW IN FRAME fMain  - widg-pos
         Customer.Cust-Num:HIDDEN IN FRAME fMain           = no.

      ASSIGN
         fll-1:HIDDEN IN FRAME fMain                       = yes
         fll-1:HEIGHT IN FRAME fMain                       = 1
         widg-pos = fll-1:ROW IN FRAME fMain 
         fll-1:ROW IN FRAME fMain                          = 4
         lbl-hndl = fll-1:SIDE-LABEL-HANDLE IN FRAME fMain 
         lbl-hndl:ROW = lbl-hndl:ROW + fll-1:ROW IN FRAME fMain  - widg-pos
         fll-1:HIDDEN IN FRAME fMain                       = no.

      ASSIGN
         Customer.Name:HIDDEN IN FRAME fMain               = yes
         Customer.Name:HEIGHT IN FRAME fMain               = 1
         widg-pos = Customer.Name:ROW IN FRAME fMain 
         Customer.Name:ROW IN FRAME fMain                  = 6
         lbl-hndl = Customer.Name:SIDE-LABEL-HANDLE IN FRAME fMain 
         lbl-hndl:ROW = lbl-hndl:ROW + Customer.Name:ROW IN FRAME fMain  - widg-pos
         Customer.Name:HIDDEN IN FRAME fMain               = no.

      ASSIGN
         Customer.Phone:HIDDEN IN FRAME fMain              = yes
         Customer.Phone:HEIGHT IN FRAME fMain              = 1
         widg-pos = Customer.Phone:ROW IN FRAME fMain 
         Customer.Phone:ROW IN FRAME fMain                 = 8
         lbl-hndl = Customer.Phone:SIDE-LABEL-HANDLE IN FRAME fMain 
         lbl-hndl:ROW = lbl-hndl:ROW + Customer.Phone:ROW IN FRAME fMain  - widg-pos
         Customer.Phone:HIDDEN IN FRAME fMain              = no.

      ASSIGN
         RECT-8:HIDDEN IN FRAME fMain                      = yes
         RECT-8:HEIGHT IN FRAME fMain                      = 1.43
         RECT-8:ROW IN FRAME fMain                         = 4
         RECT-8:WIDTH IN FRAME fMain                       = 50
         RECT-8:HIDDEN IN FRAME fMain                      = no.

      ASSIGN
         RECT-9:HIDDEN IN FRAME fMain                      = yes
         RECT-9:HEIGHT IN FRAME fMain                      = 1.43
         RECT-9:ROW IN FRAME fMain                         = 5
         RECT-9:WIDTH IN FRAME fMain                       = 50
         RECT-9:HIDDEN IN FRAME fMain                      = no.

      ASSIGN
         &IF '{&WINDOW-SYSTEM}' NE 'TTY':U &THEN
         wWin:HIDDEN                                       = no &ENDIF.

    END.  /* Standard MS Windows Layout Case */

    WHEN "Standard Windows 95":U THEN DO:
      ASSIGN
         &IF '{&WINDOW-SYSTEM}' NE 'TTY':U &THEN
         FRAME fMain:HIDDEN                                = yes &ENDIF
         FRAME fMain:HEIGHT                                = 10
         FRAME fMain:WIDTH                                 = 80 NO-ERROR.

      ASSIGN
         btn-1:HIDDEN IN FRAME fMain                       = yes
         btn-1:HEIGHT IN FRAME fMain                       = 1
         btn-1:ROW IN FRAME fMain                          = 2
         btn-1:HIDDEN IN FRAME fMain                       = no NO-ERROR.

      ASSIGN
         btn-2:HIDDEN IN FRAME fMain                       = yes
         btn-2:HEIGHT IN FRAME fMain                       = 1
         btn-2:ROW IN FRAME fMain                          = 2
         btn-2:HIDDEN IN FRAME fMain                       = no NO-ERROR.

      ASSIGN
         btn-3:HIDDEN IN FRAME fMain                       = yes
         btn-3:HEIGHT IN FRAME fMain                       = 1
         btn-3:ROW IN FRAME fMain                          = 2
         btn-3:HIDDEN IN FRAME fMain                       = no NO-ERROR.

      ASSIGN
         btn-4:HIDDEN IN FRAME fMain                       = yes
         btn-4:HEIGHT IN FRAME fMain                       = 1
         btn-4:ROW IN FRAME fMain                          = 2
         btn-4:HIDDEN IN FRAME fMain                       = no NO-ERROR.

      ASSIGN
         btn-5:HIDDEN IN FRAME fMain                       = yes
         btn-5:HEIGHT IN FRAME fMain                       = 1
         btn-5:ROW IN FRAME fMain                          = 1
         btn-5:HIDDEN IN FRAME fMain                       = no NO-ERROR.

      ASSIGN
         btn-6:HIDDEN IN FRAME fMain                       = yes
         btn-6:HEIGHT IN FRAME fMain                       = 1
         btn-6:ROW IN FRAME fMain                          = 1
         btn-6:HIDDEN IN FRAME fMain                       = no NO-ERROR.

      ASSIGN
         btn-7:HIDDEN IN FRAME fMain                       = yes
         btn-7:HEIGHT IN FRAME fMain                       = 1
         btn-7:ROW IN FRAME fMain                          = 1
         btn-7:HIDDEN IN FRAME fMain                       = no NO-ERROR.

      ASSIGN
         btn-8:HIDDEN IN FRAME fMain                       = yes
         btn-8:HEIGHT IN FRAME fMain                       = 1
         btn-8:ROW IN FRAME fMain                          = 4
         btn-8:HIDDEN IN FRAME fMain                       = no NO-ERROR.

      ASSIGN
         Customer.Contact:HIDDEN IN FRAME fMain            = yes
         Customer.Contact:HEIGHT IN FRAME fMain            = 1
         widg-pos = Customer.Contact:ROW IN FRAME fMain 
         Customer.Contact:ROW IN FRAME fMain               = 7
         lbl-hndl = Customer.Contact:SIDE-LABEL-HANDLE IN FRAME fMain 
         lbl-hndl:ROW = lbl-hndl:ROW + Customer.Contact:ROW IN FRAME fMain  - widg-pos
         Customer.Contact:HIDDEN IN FRAME fMain            = no NO-ERROR.

      ASSIGN
         Customer.Cust-Num:HIDDEN IN FRAME fMain           = yes
         Customer.Cust-Num:HEIGHT IN FRAME fMain           = 1
         widg-pos = Customer.Cust-Num:ROW IN FRAME fMain 
         Customer.Cust-Num:ROW IN FRAME fMain              = 5
         lbl-hndl = Customer.Cust-Num:SIDE-LABEL-HANDLE IN FRAME fMain 
         lbl-hndl:ROW = lbl-hndl:ROW + Customer.Cust-Num:ROW IN FRAME fMain  - widg-pos
         Customer.Cust-Num:HIDDEN IN FRAME fMain           = no NO-ERROR.

      ASSIGN
         fll-1:HIDDEN IN FRAME fMain                       = yes
         fll-1:HEIGHT IN FRAME fMain                       = 1
         widg-pos = fll-1:ROW IN FRAME fMain 
         fll-1:ROW IN FRAME fMain                          = 4
         lbl-hndl = fll-1:SIDE-LABEL-HANDLE IN FRAME fMain 
         lbl-hndl:ROW = lbl-hndl:ROW + fll-1:ROW IN FRAME fMain  - widg-pos
         fll-1:HIDDEN IN FRAME fMain                       = no NO-ERROR.

      ASSIGN
         Customer.Name:HIDDEN IN FRAME fMain               = yes
         Customer.Name:HEIGHT IN FRAME fMain               = 1
         widg-pos = Customer.Name:ROW IN FRAME fMain 
         Customer.Name:ROW IN FRAME fMain                  = 6
         lbl-hndl = Customer.Name:SIDE-LABEL-HANDLE IN FRAME fMain 
         lbl-hndl:ROW = lbl-hndl:ROW + Customer.Name:ROW IN FRAME fMain  - widg-pos
         Customer.Name:HIDDEN IN FRAME fMain               = no NO-ERROR.

      ASSIGN
         Customer.Phone:HIDDEN IN FRAME fMain              = yes
         Customer.Phone:HEIGHT IN FRAME fMain              = 1
         widg-pos = Customer.Phone:ROW IN FRAME fMain 
         Customer.Phone:ROW IN FRAME fMain                 = 8
         lbl-hndl = Customer.Phone:SIDE-LABEL-HANDLE IN FRAME fMain 
         lbl-hndl:ROW = lbl-hndl:ROW + Customer.Phone:ROW IN FRAME fMain  - widg-pos
         Customer.Phone:HIDDEN IN FRAME fMain              = no NO-ERROR.

      ASSIGN
         RECT-8:HIDDEN IN FRAME fMain                      = yes
         RECT-8:HEIGHT IN FRAME fMain                      = 1
         RECT-8:ROW IN FRAME fMain                         = 4
         RECT-8:WIDTH IN FRAME fMain                       = 50
         RECT-8:HIDDEN IN FRAME fMain                      = no NO-ERROR.

      ASSIGN
         RECT-9:HIDDEN IN FRAME fMain                      = yes
         RECT-9:HEIGHT IN FRAME fMain                      = 1
         RECT-9:ROW IN FRAME fMain                         = 5
         RECT-9:WIDTH IN FRAME fMain                       = 50
         RECT-9:HIDDEN IN FRAME fMain                      = no NO-ERROR.

      ASSIGN

         FRAME fMain:VIRTUAL-HEIGHT                        = 10.00
                    WHEN FRAME fMain:SCROLLABLE

         FRAME fMain:VIRTUAL-WIDTH                         = 80.00
                    WHEN FRAME fMain:SCROLLABLE
         &IF '{&WINDOW-SYSTEM}' NE 'TTY':U &THEN
         FRAME fMain:HIDDEN                                = no &ENDIF NO-ERROR.

    END.  /* Standard Windows 95 Layout Case */

  END CASE.
END PROCEDURE.  /* wWin-layouts */
&ANALYZE-RESUME

