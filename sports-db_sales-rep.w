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

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain
&Scoped-define BROWSE-NAME br-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Salesrep

/* Definitions for BROWSE br-1                                          */
&Scoped-define FIELDS-IN-QUERY-br-1 Salesrep.Sales-Rep Salesrep.Rep-Name ~
Salesrep.Region 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-1 
&Scoped-define QUERY-STRING-br-1 FOR EACH Salesrep NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-1 OPEN QUERY br-1 FOR EACH Salesrep NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-1 Salesrep
&Scoped-define FIRST-TABLE-IN-QUERY-br-1 Salesrep


/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-br-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Salesrep.Sales-Rep Salesrep.Rep-Name ~
Salesrep.Region Salesrep.Month-Quota[1] Salesrep.Month-Quota[2] ~
Salesrep.Month-Quota[3] Salesrep.Month-Quota[4] Salesrep.Month-Quota[5] ~
Salesrep.Month-Quota[6] Salesrep.Month-Quota[7] Salesrep.Month-Quota[8] ~
Salesrep.Month-Quota[9] Salesrep.Month-Quota[10] Salesrep.Month-Quota[11] ~
Salesrep.Month-Quota[12] 
&Scoped-define ENABLED-TABLES Salesrep
&Scoped-define FIRST-ENABLED-TABLE Salesrep
&Scoped-Define ENABLED-OBJECTS br-1 btn-5 btn-7 btn-6 RECT-14 RECT-15 ~
RECT-16 
&Scoped-Define DISPLAYED-FIELDS Salesrep.Sales-Rep Salesrep.Rep-Name ~
Salesrep.Region Salesrep.Month-Quota[1] Salesrep.Month-Quota[2] ~
Salesrep.Month-Quota[3] Salesrep.Month-Quota[4] Salesrep.Month-Quota[5] ~
Salesrep.Month-Quota[6] Salesrep.Month-Quota[7] Salesrep.Month-Quota[8] ~
Salesrep.Month-Quota[9] Salesrep.Month-Quota[10] Salesrep.Month-Quota[11] ~
Salesrep.Month-Quota[12] 
&Scoped-define DISPLAYED-TABLES Salesrep
&Scoped-define FIRST-DISPLAYED-TABLE Salesrep


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-5 
     LABEL "&Save" 
     SIZE 11 BY .95.

DEFINE BUTTON btn-6 
     LABEL "&New" 
     SIZE 11 BY .95.

DEFINE BUTTON btn-7 
     LABEL "&Delete" 
     SIZE 11 BY .95.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 86 BY 8.33.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 86 BY 3.81.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 86 BY 7.38.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-1 FOR 
      Salesrep SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-1 wWin _STRUCTURED
  QUERY br-1 NO-LOCK DISPLAY
      Salesrep.Sales-Rep FORMAT "x(4)":U WIDTH 16.2
      Salesrep.Rep-Name FORMAT "x(30)":U
      Salesrep.Region FORMAT "x(8)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 84 BY 7.86 ROW-HEIGHT-CHARS .81 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     br-1 AT ROW 2.91 COL 3 WIDGET-ID 200
     btn-5 AT ROW 1.48 COL 53 WIDGET-ID 46
     btn-7 AT ROW 1.48 COL 65 WIDGET-ID 50
     btn-6 AT ROW 1.48 COL 77 WIDGET-ID 48
     Salesrep.Sales-Rep AT ROW 11.95 COL 13 COLON-ALIGNED WIDGET-ID 80
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     Salesrep.Rep-Name AT ROW 12.91 COL 13 COLON-ALIGNED WIDGET-ID 78
          VIEW-AS FILL-IN 
          SIZE 71 BY 1
     Salesrep.Region AT ROW 13.86 COL 13 COLON-ALIGNED WIDGET-ID 76
          VIEW-AS FILL-IN 
          SIZE 71 BY 1
     Salesrep.Month-Quota[1] AT ROW 16 COL 13 COLON-ALIGNED WIDGET-ID 88
          LABEL "January"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     Salesrep.Month-Quota[2] AT ROW 16.95 COL 13 COLON-ALIGNED WIDGET-ID 90
          LABEL "February"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     Salesrep.Month-Quota[3] AT ROW 17.91 COL 13 COLON-ALIGNED WIDGET-ID 92
          LABEL "March"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     Salesrep.Month-Quota[4] AT ROW 19.33 COL 13 COLON-ALIGNED WIDGET-ID 94
          LABEL "April"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     Salesrep.Month-Quota[5] AT ROW 20.29 COL 13 COLON-ALIGNED WIDGET-ID 96
          LABEL "May"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     Salesrep.Month-Quota[6] AT ROW 21.24 COL 13 COLON-ALIGNED WIDGET-ID 98
          LABEL "June"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     Salesrep.Month-Quota[7] AT ROW 16 COL 58 COLON-ALIGNED WIDGET-ID 100
          LABEL "July"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     Salesrep.Month-Quota[8] AT ROW 16.95 COL 58 COLON-ALIGNED WIDGET-ID 102
          LABEL "August"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     Salesrep.Month-Quota[9] AT ROW 17.91 COL 58 COLON-ALIGNED WIDGET-ID 104
          LABEL "September"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     Salesrep.Month-Quota[10] AT ROW 19.33 COL 58 COLON-ALIGNED WIDGET-ID 82
          LABEL "October"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     Salesrep.Month-Quota[11] AT ROW 20.29 COL 58 COLON-ALIGNED WIDGET-ID 84
          LABEL "November"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     Salesrep.Month-Quota[12] AT ROW 21.24 COL 58 COLON-ALIGNED WIDGET-ID 86
          LABEL "December"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     "sports-db_sales-rep.w [v1.00]" VIEW-AS TEXT
          SIZE 29 BY .95 AT ROW 22.91 COL 2 WIDGET-ID 116
     "Monthly Quote:" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 15.29 COL 3 WIDGET-ID 114
     "Sales Representative:" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 11.24 COL 3 WIDGET-ID 112
     "Sales Rep. Management" VIEW-AS TEXT
          SIZE 48 BY 1.67 AT ROW 1 COL 3 WIDGET-ID 2
          FONT 43
     RECT-14 AT ROW 2.67 COL 2 WIDGET-ID 106
     RECT-15 AT ROW 11.48 COL 2 WIDGET-ID 108
     RECT-16 AT ROW 15.52 COL 2 WIDGET-ID 110
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 88 BY 22.95 WIDGET-ID 100.


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
         TITLE              = "Sales Representative Management"
         HEIGHT             = 22.95
         WIDTH              = 88
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
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB br-1 1 fMain */
/* SETTINGS FOR FILL-IN Salesrep.Month-Quota[10] IN FRAME fMain
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Salesrep.Month-Quota[11] IN FRAME fMain
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Salesrep.Month-Quota[12] IN FRAME fMain
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Salesrep.Month-Quota[1] IN FRAME fMain
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Salesrep.Month-Quota[2] IN FRAME fMain
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Salesrep.Month-Quota[3] IN FRAME fMain
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Salesrep.Month-Quota[4] IN FRAME fMain
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Salesrep.Month-Quota[5] IN FRAME fMain
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Salesrep.Month-Quota[6] IN FRAME fMain
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Salesrep.Month-Quota[7] IN FRAME fMain
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Salesrep.Month-Quota[8] IN FRAME fMain
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Salesrep.Month-Quota[9] IN FRAME fMain
   EXP-LABEL                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-1
/* Query rebuild information for BROWSE br-1
     _TblList          = "sports.Salesrep"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > sports.Salesrep.Sales-Rep
"Salesrep.Sales-Rep" ? ? "character" ? ? ? ? ? ? no ? no no "16.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = sports.Salesrep.Rep-Name
     _FldNameList[3]   = sports.Salesrep.Region
     _Query            is OPENED
*/  /* BROWSE br-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Sales Representative Management */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Sales Representative Management */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-1
&Scoped-define SELF-NAME br-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-1 wWin
ON DEL OF br-1 IN FRAME fMain
DO:
    RUN RecDelete.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-1 wWin
ON VALUE-CHANGED OF br-1 IN FRAME fMain
DO:
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

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
  IF AVAILABLE Salesrep THEN 
    DISPLAY Salesrep.Sales-Rep Salesrep.Rep-Name Salesrep.Region 
          Salesrep.Month-Quota[1] Salesrep.Month-Quota[2] 
          Salesrep.Month-Quota[3] Salesrep.Month-Quota[4] 
          Salesrep.Month-Quota[5] Salesrep.Month-Quota[6] 
          Salesrep.Month-Quota[7] Salesrep.Month-Quota[8] 
          Salesrep.Month-Quota[9] Salesrep.Month-Quota[10] 
          Salesrep.Month-Quota[11] Salesrep.Month-Quota[12] 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE br-1 btn-5 btn-7 btn-6 Salesrep.Sales-Rep Salesrep.Rep-Name 
         Salesrep.Region Salesrep.Month-Quota[1] Salesrep.Month-Quota[2] 
         Salesrep.Month-Quota[3] Salesrep.Month-Quota[4] 
         Salesrep.Month-Quota[5] Salesrep.Month-Quota[6] 
         Salesrep.Month-Quota[7] Salesrep.Month-Quota[8] 
         Salesrep.Month-Quota[9] Salesrep.Month-Quota[10] 
         Salesrep.Month-Quota[11] Salesrep.Month-Quota[12] RECT-14 RECT-15 
         RECT-16 
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

    FIND CURRENT salesrep EXCLUSIVE-LOCK.

    CASE lChoice:
        WHEN TRUE THEN DO:
            DELETE salesrep.

            ASSIGN salesrep.sales-rep:SCREEN-VALUE = ""
                    salesrep.rep-name:SCREEN-VALUE = ""
                    salesrep.region:SCREEN-VALUE = ""
                    salesrep.month-quota[1]:SCREEN-VALUE = ""
                    salesrep.month-quota[2]:SCREEN-VALUE = ""
                    salesrep.month-quota[3]:SCREEN-VALUE = ""
                    salesrep.month-quota[4]:SCREEN-VALUE = ""
                    salesrep.month-quota[5]:SCREEN-VALUE = ""
                    salesrep.month-quota[6]:SCREEN-VALUE = ""
                    salesrep.month-quota[7]:SCREEN-VALUE = ""
                    salesrep.month-quota[8]:SCREEN-VALUE = ""
                    salesrep.month-quota[9]:SCREEN-VALUE = ""
                    salesrep.month-quota[10]:SCREEN-VALUE = ""
                    salesrep.month-quota[11]:SCREEN-VALUE = ""
                    salesrep.month-quota[12]:SCREEN-VALUE = "".
        END.
        WHEN FALSE THEN DO:
            MESSAGE "Operation skipped."
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
                TITLE "Operation skipped".
            RETURN NO-APPLY.
        END. 
            
    END.
END.

{&OPEN-QUERY-br-1}
/* {&OPEN-QUERY-br-1} */
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

DO WITH FRAME {&FRAME-NAME}:

    CREATE salesrep.
        ASSIGN sales-rep = "NEW"
                rep-name = "New Rep".
    
    {&OPEN-QUERY-br-1}
    RUN RecShow.

END.
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
    FIND CURRENT salesrep EXCLUSIVE-LOCK.

    ASSIGN salesrep.sales-rep
        salesrep.rep-name
        salesrep.region
        salesrep.month-quota[1]
        salesrep.month-quota[2]
        salesrep.month-quota[3]
        salesrep.month-quota[4]
        salesrep.month-quota[5]
        salesrep.month-quota[6]
        salesrep.month-quota[7]
        salesrep.month-quota[8]
        salesrep.month-quota[9]
        salesrep.month-quota[10]
        salesrep.month-quota[11]
        salesrep.month-quota[12].

    MESSAGE "Saved Succesfully!"
        VIEW-AS ALERT-BOX INFORMATION
        TITLE "Saved".

    br-1:REFRESH().
    RUN RecShow.

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
DO WITH FRAME {&FRAME-NAME}:

FIND CURRENT salesrep EXCLUSIVE-LOCK.

DISPLAY salesrep.sales-rep
    salesrep.rep-name
    salesrep.region
    salesrep.month-quota[1]
    salesrep.month-quota[2]
    salesrep.month-quota[3]
    salesrep.month-quota[4]
    salesrep.month-quota[5]
    salesrep.month-quota[6]
    salesrep.month-quota[7]
    salesrep.month-quota[8]
    salesrep.month-quota[9]
    salesrep.month-quota[10]
    salesrep.month-quota[11]
    salesrep.month-quota[12].
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

