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
&Scoped-define INTERNAL-TABLES Order-Line

/* Definitions for BROWSE br-1                                          */
&Scoped-define FIELDS-IN-QUERY-br-1 Order-Line.Order-num ~
Order-Line.Extended-Price 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-1 
&Scoped-define QUERY-STRING-br-1 FOR EACH Order-Line NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-1 OPEN QUERY br-1 FOR EACH Order-Line NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-1 Order-Line
&Scoped-define FIRST-TABLE-IN-QUERY-br-1 Order-Line


/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-br-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Order-Line.Order-num Order-Line.Line-num ~
Order-Line.Backorder Order-Line.Item-num Order-Line.Price Order-Line.Qty ~
Order-Line.Discount Order-Line.Extended-Price 
&Scoped-define ENABLED-TABLES Order-Line
&Scoped-define FIRST-ENABLED-TABLE Order-Line
&Scoped-Define ENABLED-OBJECTS RECT-19 btn-5 btn-7 btn-6 br-1 
&Scoped-Define DISPLAYED-FIELDS Order-Line.Order-num Order-Line.Line-num ~
Order-Line.Backorder Order-Line.Item-num Order-Line.Price Order-Line.Qty ~
Order-Line.Discount Order-Line.Extended-Price 
&Scoped-define DISPLAYED-TABLES Order-Line
&Scoped-define FIRST-DISPLAYED-TABLE Order-Line


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

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 9.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-1 FOR 
      Order-Line SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-1 wWin _STRUCTURED
  QUERY br-1 NO-LOCK DISPLAY
      Order-Line.Order-num FORMAT ">>>>9":U WIDTH 12.2
      Order-Line.Extended-Price FORMAT "->>,>>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 33 BY 9.05 ROW-HEIGHT-CHARS .57 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     btn-5 AT ROW 1.71 COL 37 WIDGET-ID 46
     btn-7 AT ROW 1.71 COL 49 WIDGET-ID 50
     btn-6 AT ROW 1.71 COL 61 WIDGET-ID 48
     br-1 AT ROW 3.38 COL 2 WIDGET-ID 200
     Order-Line.Order-num AT ROW 4.1 COL 52 COLON-ALIGNED WIDGET-ID 128
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     Order-Line.Line-num AT ROW 5.05 COL 52 COLON-ALIGNED WIDGET-ID 126
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     Order-Line.Backorder AT ROW 6 COL 52 COLON-ALIGNED WIDGET-ID 118
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     Order-Line.Item-num AT ROW 7.19 COL 52 COLON-ALIGNED WIDGET-ID 124
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     Order-Line.Price AT ROW 8.14 COL 52 COLON-ALIGNED WIDGET-ID 130
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Order-Line.Qty AT ROW 9.1 COL 52 COLON-ALIGNED WIDGET-ID 132
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     Order-Line.Discount AT ROW 10.29 COL 52 COLON-ALIGNED WIDGET-ID 120
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     Order-Line.Extended-Price AT ROW 11.24 COL 52 COLON-ALIGNED WIDGET-ID 122
          LABEL "Total-Price"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     "Item Detail:" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 3.14 COL 37 WIDGET-ID 136
     "Order Line" VIEW-AS TEXT
          SIZE 22 BY 1.67 AT ROW 1.24 COL 2 WIDGET-ID 2
          FONT 43
     "sports-db_order-line.w [v1.00]" VIEW-AS TEXT
          SIZE 31 BY .95 AT ROW 12.91 COL 2 WIDGET-ID 116
     RECT-19 AT ROW 3.38 COL 36 WIDGET-ID 134
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 73.4 BY 13.14 WIDGET-ID 100.


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
         TITLE              = "Order Management"
         HEIGHT             = 13.14
         WIDTH              = 73.4
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
   FRAME-NAME                                                           */
/* BROWSE-TAB br-1 btn-6 fMain */
/* SETTINGS FOR FILL-IN Order-Line.Extended-Price IN FRAME fMain
   EXP-LABEL                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-1
/* Query rebuild information for BROWSE br-1
     _TblList          = "sports.Order-Line"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > sports.Order-Line.Order-num
"Order-Line.Order-num" ? ? "integer" ? ? ? ? ? ? no ? no no "12.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = sports.Order-Line.Extended-Price
     _Query            is OPENED
*/  /* BROWSE br-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Order Management */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Order Management */
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
  IF AVAILABLE Order-Line THEN 
    DISPLAY Order-Line.Order-num Order-Line.Line-num Order-Line.Backorder 
          Order-Line.Item-num Order-Line.Price Order-Line.Qty 
          Order-Line.Discount Order-Line.Extended-Price 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-19 btn-5 btn-7 btn-6 br-1 Order-Line.Order-num 
         Order-Line.Line-num Order-Line.Backorder Order-Line.Item-num 
         Order-Line.Price Order-Line.Qty Order-Line.Discount 
         Order-Line.Extended-Price 
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

    FIND CURRENT order-line EXCLUSIVE-LOCK.

    CASE lChoice:
        WHEN TRUE THEN DO:
            DELETE ITEM.

            ASSIGN order-line.order-num:SCREEN-VALUE = ""
                    order-line.line-num:SCREEN-VALUE = ""
                    order-line.backorder:SCREEN-VALUE = ""
                    order-line.discount:SCREEN-VALUE = ""
                    order-line.extended-price:SCREEN-VALUE = ""
                    order-line.item-num:SCREEN-VALUE = ""
                    order-line.price:SCREEN-VALUE = ""
                    order-line.qty:SCREEN-VALUE = "".
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

    CREATE order-line.
    
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
    FIND CURRENT order-line EXCLUSIVE-LOCK.

    ASSIGN order-line.order-num
            order-line.line-num
            order-line.backorder
            order-line.discount
            order-line.extended-price
            order-line.item-num
            order-line.price
            order-line.qty.

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

FIND CURRENT order-line EXCLUSIVE-LOCK.

DISPLAY order-line.order-num
            order-line.line-num
            order-line.backorder
            order-line.discount
            order-line.extended-price
            order-line.item-num
            order-line.price
            order-line.qty.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

