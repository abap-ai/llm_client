CLASS ltcl_llm_encryption_test DEFINITION
  FOR TESTING
  DURATION SHORT
  FINAL
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS setup.
    METHODS encrypt_decrypt_works FOR TESTING.
    METHODS encrypt_empty_string FOR TESTING.
    METHODS encrypt_long_string FOR TESTING.

    DATA encryption TYPE REF TO zif_llm_encryption.
ENDCLASS.

CLASS ltcl_llm_encryption_test IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT encryption TYPE zcl_llm_encryption.
  ENDMETHOD.

  METHOD encrypt_decrypt_works.
    " Given
    DATA original_text TYPE string.
        DATA encrypted_text TYPE xstring.
        DATA decrypted_text TYPE string.
    original_text = `Hello, World! This is a test message`.

    " When
    TRY.
        
        encrypted_text = encryption->encrypt( original_text ).
        
        decrypted_text = encryption->decrypt( encrypted_text ).
      CATCH zcx_llm_authorization.
        cl_abap_unit_assert=>fail( 'Unexpected authorization error' ).
    ENDTRY.

    " Then
    cl_abap_unit_assert=>assert_not_initial( encrypted_text ).
    cl_abap_unit_assert=>assert_equals(
      exp = original_text
      act = decrypted_text
      msg = 'Decrypted text should match original text'
    ).
  ENDMETHOD.

  METHOD encrypt_empty_string.
    " Given
    DATA original_text TYPE string.
        DATA encrypted_text TYPE xstring.
        DATA decrypted_text TYPE string.
    original_text = ``.

    " When
    TRY.
        
        encrypted_text = encryption->encrypt( original_text ).
        
        decrypted_text = encryption->decrypt( encrypted_text ).
      CATCH zcx_llm_authorization.
        cl_abap_unit_assert=>fail( 'Unexpected authorization error' ).
    ENDTRY.

    " Then
    cl_abap_unit_assert=>assert_equals(
      exp = original_text
      act = decrypted_text
      msg = 'Empty string should be handled correctly'
    ).
  ENDMETHOD.

  METHOD encrypt_long_string.
    " Given
    DATA original_text TYPE string.
        DATA encrypted_text TYPE xstring.
        DATA decrypted_text TYPE string.
    original_text = repeat(
      val = 'This is a very long test string that exceeds standard buffer sizes '
      occ = 100
    ).

    " When
    TRY.
        
        encrypted_text = encryption->encrypt( original_text ).
        
        decrypted_text = encryption->decrypt( encrypted_text ).
      CATCH zcx_llm_authorization.
        cl_abap_unit_assert=>fail( 'Unexpected authorization error' ).
    ENDTRY.

    " Then
    cl_abap_unit_assert=>assert_equals(
      exp = original_text
      act = decrypted_text
      msg = 'Long string should be encrypted and decrypted correctly'
    ).
  ENDMETHOD.

ENDCLASS.
