*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_Z_LLM_CLIENT
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_Z_LLM_CLIENT       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.