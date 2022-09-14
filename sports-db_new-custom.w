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
&Scoped-define INTERNAL-TABLES Customer

/* Definitions for BROWSE br-1                                          */
&Scoped-define FIELDS-IN-QUERY-br-1 Customer.Cust-Num Customer.Name ~
Customer.Contact Customer.Phone Customer.Address Customer.Postal-Code ~
Customer.Country 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-1 
&Scoped-define QUERY-STRING-br-1 FOR EACH Customer NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-1 OPEN QUERY br-1 FOR EACH Customer NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-1 Customer
&Scoped-define FIRST-TABLE-IN-QUERY-br-1 Customer


/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-br-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Customer.Cust-Num Customer.Name ~
Customer.Contact Customer.Phone Customer.Address Customer.Address2 ~
Customer.Postal-Code Customer.City Customer.State Customer.Country ~
Customer.Credit-Limit Customer.Balance Customer.Discount Customer.Sales-Rep ~
Customer.Terms Customer.Comments 
&Scoped-define ENABLED-TABLES Customer
&Scoped-define FIRST-ENABLED-TABLE Customer
&Scoped-Define ENABLED-OBJECTS br-1 btn-6 btn-7 btn-5 RECT-10 RECT-11 ~
RECT-12 RECT-13 
&Scoped-Define DISPLAYED-FIELDS Customer.Cust-Num Customer.Name ~
Customer.Contact Customer.Phone Customer.Address Customer.Address2 ~
Customer.Postal-Code Customer.City Customer.State Customer.Country ~
Customer.Credit-Limit Customer.Balance Customer.Discount Customer.Sales-Rep ~
Customer.Terms Customer.Comments 
&Scoped-define DISPLAYED-TABLES Customer
&Scoped-define FIRST-DISPLAYED-TABLE Customer


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

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 151 BY .1.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 151 BY .1.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 150 BY 8.33.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 150 BY 15.95.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-1 FOR 
      Customer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-1 wWin _STRUCTURED
  QUERY br-1 NO-LOCK DISPLAY
      Customer.Cust-Num FORMAT ">>>>9":U
      Customer.Name FORMAT "x(20)":U
      Customer.Contact FORMAT "x(20)":U
      Customer.Phone FORMAT "x(20)":U
      Customer.Address FORMAT "x(20)":U
      Customer.Postal-Code FORMAT "x(10)":U WIDTH 16
      Customer.Country FORMAT "x(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 148 BY 15.48 ROW-HEIGHT-CHARS .71 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     br-1 AT ROW 4.1 COL 3 WIDGET-ID 200
     btn-6 AT ROW 1.71 COL 117 WIDGET-ID 48
     btn-7 AT ROW 1.71 COL 129 WIDGET-ID 50
     btn-5 AT ROW 1.71 COL 141 WIDGET-ID 46
     Customer.Cust-Num AT ROW 21.71 COL 13 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Customer.Name AT ROW 22.67 COL 13 COLON-ALIGNED WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
     Customer.Contact AT ROW 23.62 COL 13 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
     Customer.Phone AT ROW 24.57 COL 13 COLON-ALIGNED WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
     Customer.Address AT ROW 21.71 COL 63 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
     Customer.Address2 AT ROW 22.67 COL 63 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
     Customer.Postal-Code AT ROW 23.62 COL 63 COLON-ALIGNED WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
     Customer.City AT ROW 24.57 COL 63 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
     Customer.State AT ROW 25.52 COL 63 COLON-ALIGNED WIDGET-ID 38
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
     Customer.Country AT ROW 26.48 COL 63 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
     Customer.Credit-Limit AT ROW 21.71 COL 114 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
     Customer.Balance AT ROW 22.67 COL 114 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
     Customer.Discount AT ROW 23.62 COL 114 COLON-ALIGNED WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
     Customer.Sales-Rep AT ROW 25.52 COL 114 COLON-ALIGNED WIDGET-ID 36
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
     Customer.Terms AT ROW 26.48 COL 114 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
     Customer.Comments AT ROW 27.91 COL 13 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 135 BY 1
     "sports-db_new-custom.w [v1.00]" VIEW-AS TEXT
          SIZE 31 BY .95 AT ROW 29.33 COL 2 WIDGET-ID 52
     "Customer Management" VIEW-AS TEXT
          SIZE 49 BY 1.43 AT ROW 1.48 COL 3 WIDGET-ID 2
          FONT 43
     "Customer:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 20.76 COL 4 WIDGET-ID 42
     RECT-10 AT ROW 3.38 COL 1 WIDGET-ID 4
     RECT-11 AT ROW 20.29 COL 1 WIDGET-ID 6
     RECT-12 AT ROW 21 COL 2 WIDGET-ID 8
     RECT-13 AT ROW 3.86 COL 2 WIDGET-ID 44
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 151.6 BY 29.33 WIDGET-ID 100.


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
         TITLE              = "Customer Management"
         HEIGHT             = 29.33
         WIDTH              = 151.6
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
ASSIGN 
       Customer.Cust-Num:READ-ONLY IN FRAME fMain        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-1
/* Query rebuild information for BROWSE br-1
     _TblList          = "sports.Customer"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = sports.Customer.Cust-Num
     _FldNameList[2]   = sports.Customer.Name
     _FldNameList[3]   = sports.Customer.Contact
     _FldNameList[4]   = sports.Customer.Phone
     _FldNameList[5]   = sports.Customer.Address
     _FldNameList[6]   > sports.Customer.Postal-Code
"Customer.Postal-Code" ? ? "character" ? ? ? ? ? ? no ? no no "16" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   = sports.Customer.Country
     _Query            is OPENED
*/  /* BROWSE br-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Customer Management */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Customer Management */
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
  IF AVAILABLE Customer THEN 
    DISPLAY Customer.Cust-Num Customer.Name Customer.Contact Customer.Phone 
          Customer.Address Customer.Address2 Customer.Postal-Code Customer.City 
          Customer.State Customer.Country Customer.Credit-Limit Customer.Balance 
          Customer.Discount Customer.Sales-Rep Customer.Terms Customer.Comments 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE br-1 btn-6 btn-7 btn-5 Customer.Cust-Num Customer.Name 
         Customer.Contact Customer.Phone Customer.Address Customer.Address2 
         Customer.Postal-Code Customer.City Customer.State Customer.Country 
         Customer.Credit-Limit Customer.Balance Customer.Discount 
         Customer.Sales-Rep Customer.Terms Customer.Comments RECT-10 RECT-11 
         RECT-12 RECT-13 
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

    MESSAGE "Do you want to delete the register?" 
        SKIP "This action can't be undone."
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
    TITLE "Delete registry" UPDATE lChoice AS LOGICAL.

    FIND CURRENT customer EXCLUSIVE-LOCK.

    CASE lChoice:
        WHEN TRUE THEN DO:
            DELETE customer.

            ASSIGN customer.cust-num:SCREEN-VALUE = ""
                customer.NAME:SCREEN-VALUE = ""
                customer.contact:SCREEN-VALUE = ""
                customer.phone:SCREEN-VALUE = ""
                customer.address:SCREEN-VALUE = ""
                customer.address2:SCREEN-VALUE = ""
                customer.postal-code:SCREEN-VALUE = ""
                customer.city:SCREEN-VALUE = ""
                customer.state:SCREEN-VALUE = ""
                customer.country:SCREEN-VALUE = ""
                customer.credit-limit:SCREEN-VALUE = ""
                customer.balance:SCREEN-VALUE = ""
                customer.discount:SCREEN-VALUE = ""
                customer.sales-rep:SCREEN-VALUE = ""
                customer.terms:SCREEN-VALUE = ""
                customer.comments:SCREEN-VALUE = "".
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

    CREATE customer.
        ASSIGN NAME = "New User".
    
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
    FIND CURRENT customer EXCLUSIVE-LOCK.

    ASSIGN customer.cust-num
            customer.NAME
            customer.contact
            customer.phone
            customer.address
            customer.address2
            customer.postal-code
            customer.city
            customer.state
            customer.country
            customer.credit-limit
            customer.balance
            customer.discount
            customer.sales-rep
            customer.terms
            customer.comments.

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

FIND CURRENT customer EXCLUSIVE-LOCK.

DISPLAY customer.cust-num
    customer.NAME
    customer.contact
    customer.phone
    customer.address
    customer.address2
    customer.postal-code
    customer.city
    customer.state
    customer.country
    customer.credit-limit
    customer.balance
    customer.discount
    customer.sales-rep
    customer.terms
    customer.comments.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

