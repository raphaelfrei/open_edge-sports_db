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
&Scoped-define INTERNAL-TABLES Invoice

/* Definitions for BROWSE br-1                                          */
&Scoped-define FIELDS-IN-QUERY-br-1 Invoice.Invoice-Num Invoice.Cust-Num ~
Invoice.Order-Num Invoice.Invoice-Date Invoice.Amount Invoice.Total-Paid ~
Invoice.Ship-Charge 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-1 
&Scoped-define QUERY-STRING-br-1 FOR EACH Invoice NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-1 OPEN QUERY br-1 FOR EACH Invoice NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-1 Invoice
&Scoped-define FIRST-TABLE-IN-QUERY-br-1 Invoice


/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-br-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Invoice.Invoice-Num Invoice.Invoice-Date ~
Invoice.Ship-Charge Invoice.Adjustment Invoice.Cust-Num Invoice.Order-Num ~
Invoice.Amount Invoice.Total-Paid 
&Scoped-define ENABLED-TABLES Invoice
&Scoped-define FIRST-ENABLED-TABLE Invoice
&Scoped-Define ENABLED-OBJECTS RECT-22 RECT-23 btn-5 btn-7 btn-6 br-1 
&Scoped-Define DISPLAYED-FIELDS Invoice.Invoice-Num Invoice.Invoice-Date ~
Invoice.Ship-Charge Invoice.Adjustment Invoice.Cust-Num Invoice.Order-Num ~
Invoice.Amount Invoice.Total-Paid 
&Scoped-define DISPLAYED-TABLES Invoice
&Scoped-define FIRST-DISPLAYED-TABLE Invoice


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

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 124 BY 13.57.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 124 BY 2.86.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-1 FOR 
      Invoice SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-1 wWin _STRUCTURED
  QUERY br-1 NO-LOCK DISPLAY
      Invoice.Invoice-Num FORMAT ">>>>>9":U WIDTH 16.2
      Invoice.Cust-Num FORMAT ">>>>9":U WIDTH 20.2
      Invoice.Order-Num FORMAT ">>>>9":U WIDTH 17.2
      Invoice.Invoice-Date FORMAT "99/99/99":U WIDTH 15.2
      Invoice.Amount FORMAT "->>,>>9.99":U WIDTH 14
      Invoice.Total-Paid FORMAT "->>,>>9.99":U WIDTH 12.2
      Invoice.Ship-Charge FORMAT "->>,>>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 122 BY 13.1 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     btn-5 AT ROW 1.48 COL 90 WIDGET-ID 46
     btn-7 AT ROW 1.48 COL 102 WIDGET-ID 50
     btn-6 AT ROW 1.48 COL 114 WIDGET-ID 48
     br-1 AT ROW 3.14 COL 3 WIDGET-ID 200
     Invoice.Invoice-Num AT ROW 17.43 COL 15 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     Invoice.Invoice-Date AT ROW 17.43 COL 44 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     Invoice.Ship-Charge AT ROW 17.43 COL 73 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     Invoice.Adjustment AT ROW 17.43 COL 107 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     Invoice.Cust-Num AT ROW 18.38 COL 15 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     Invoice.Order-Num AT ROW 18.43 COL 44 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     Invoice.Amount AT ROW 18.43 COL 73 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     Invoice.Total-Paid AT ROW 18.43 COL 107 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     "Invoices" VIEW-AS TEXT
          SIZE 48 BY 1.67 AT ROW 1.24 COL 3 WIDGET-ID 52
          FONT 43
     "Invoice Adjustment:" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 16.71 COL 3 WIDGET-ID 58
     "sports-db_invoice.w [v1.00]" VIEW-AS TEXT
          SIZE 29 BY .95 AT ROW 19.81 COL 2 WIDGET-ID 116
     RECT-22 AT ROW 2.91 COL 2 WIDGET-ID 54
     RECT-23 AT ROW 16.95 COL 2 WIDGET-ID 56
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125.8 BY 19.81 WIDGET-ID 100.


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
         TITLE              = "Invoices"
         HEIGHT             = 19.81
         WIDTH              = 125.8
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.2
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.2
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
   FRAME-NAME                                                           */
/* BROWSE-TAB br-1 btn-6 fMain */
ASSIGN 
       Invoice.Invoice-Num:READ-ONLY IN FRAME fMain        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-1
/* Query rebuild information for BROWSE br-1
     _TblList          = "sports.Invoice"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > sports.Invoice.Invoice-Num
"Invoice.Invoice-Num" ? ? "integer" ? ? ? ? ? ? no ? no no "16.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > sports.Invoice.Cust-Num
"Invoice.Cust-Num" ? ? "integer" ? ? ? ? ? ? no ? no no "20.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > sports.Invoice.Order-Num
"Invoice.Order-Num" ? ? "integer" ? ? ? ? ? ? no ? no no "17.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > sports.Invoice.Invoice-Date
"Invoice.Invoice-Date" ? ? "date" ? ? ? ? ? ? no ? no no "15.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > sports.Invoice.Amount
"Invoice.Amount" ? ? "decimal" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > sports.Invoice.Total-Paid
"Invoice.Total-Paid" ? ? "decimal" ? ? ? ? ? ? no ? no no "12.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   = sports.Invoice.Ship-Charge
     _Query            is OPENED
*/  /* BROWSE br-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Invoices */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Invoices */
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
  IF AVAILABLE Invoice THEN 
    DISPLAY Invoice.Invoice-Num Invoice.Invoice-Date Invoice.Ship-Charge 
          Invoice.Adjustment Invoice.Cust-Num Invoice.Order-Num Invoice.Amount 
          Invoice.Total-Paid 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-22 RECT-23 btn-5 btn-7 btn-6 br-1 Invoice.Invoice-Num 
         Invoice.Invoice-Date Invoice.Ship-Charge Invoice.Adjustment 
         Invoice.Cust-Num Invoice.Order-Num Invoice.Amount Invoice.Total-Paid 
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

    FIND CURRENT invoice EXCLUSIVE-LOCK.

    CASE lChoice:
        WHEN TRUE THEN DO:
            DELETE invoice.

            ASSIGN invoice.invoice-num:SCREEN-VALUE = ""
                invoice.cust-num:SCREEN-VALUE = ""
                invoice.invoice-date:SCREEN-VALUE = ""
                invoice.order-num:SCREEN-VALUE = ""
                invoice.adjustment:SCREEN-VALUE = ""
                invoice.amount:SCREEN-VALUE = ""
                invoice.ship-charge:SCREEN-VALUE = ""
                invoice.total-paid:SCREEN-VALUE = "".
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

    CREATE invoice.
    
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
    FIND CURRENT invoice EXCLUSIVE-LOCK.

    ASSIGN invoice.invoice-num
    invoice.cust-num
    invoice.invoice-date
    invoice.order-num
    invoice.adjustment
    invoice.amount
    invoice.ship-charge
    invoice.total-paid.

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

FIND CURRENT invoice EXCLUSIVE-LOCK.

DISPLAY invoice.invoice-num
    invoice.cust-num
    invoice.invoice-date
    invoice.order-num
    invoice.adjustment
    invoice.amount
    invoice.ship-charge
    invoice.total-paid.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

