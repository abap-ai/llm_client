"! <p class="shorttext synchronized" lang="en">Agent Manager Interface</p>
INTERFACE zif_llm_agent_manager
  PUBLIC.

  CONSTANTS:
    "! Execution strategy types
    BEGIN OF execution_strategy,
      sequential TYPE string VALUE 'SEQUENTIAL',
      parallel   TYPE string VALUE 'PARALLEL',
      consensus  TYPE string VALUE 'CONSENSUS',  " Combine results from multiple agents
    END OF execution_strategy.

  TYPES:
    "! Manager configuration
    BEGIN OF configuration,
      max_iterations TYPE i,
      timeout        TYPE i,
      system_prompt  TYPE string,
      "! Execution strategy to use
      strategy       TYPE string,
      "! Minimum agents required for consensus (for consensus strategy)
      min_consensus  TYPE i,
      "! Success criteria for competing strategy
      success_cond   TYPE string,
    END OF configuration.
  TYPES:
    "! Workflow step definition
    BEGIN OF workflow_step,
      agent_name TYPE string,
      "! Optional group for parallel/consensus execution
      group_id   TYPE string,
      sequence   TYPE i,
      "! Dependencies on other steps
      depends_on TYPE string_table,
      input_map  TYPE string,
      "! Weight for consensus calculation
      weight     TYPE decfloat16,
    END OF workflow_step,
    "! Workflow definition
    workflow_steps TYPE STANDARD TABLE OF workflow_step WITH KEY sequence.
  TYPES:
    "! Execution summary
    BEGIN OF execution_summary,
      start_time    TYPE timestamp,
      end_time      TYPE timestamp,
      steps_total   TYPE i,
      steps_done    TYPE i,
      has_error     TYPE abap_bool,
      error_message TYPE string,
      result        TYPE zllm_response,
      "! Results from individual agents for consensus/competing strategies
      agent_results TYPE STANDARD TABLE OF zllm_response WITH DEFAULT KEY,
    END OF execution_summary.

  "! <p class="shorttext synchronized">Registers an agent for the workflow</p>
  "! @parameter agent               | <p class="shorttext synchronized">Agent to register</p>
  "! @parameter name                | <p class="shorttext synchronized">Name for workflow reference</p>
  "! @raising   zcx_llm_agent_error | <p class="shorttext synchronized">Registration error</p>
  METHODS register_agent
    IMPORTING agent TYPE REF TO zif_llm_agent
              !name TYPE string
    RAISING   zcx_llm_agent_error.

  "! <p class="shorttext synchronized">Defines workflow steps with execution strategy</p>
  "! @parameter steps               | <p class="shorttext synchronized">Workflow step definitions</p>
  "! @raising   zcx_llm_agent_error | <p class="shorttext synchronized">Invalid workflow definition</p>
  METHODS set_workflow
    IMPORTING steps TYPE workflow_steps
    RAISING   zcx_llm_agent_error.

  "! <p class="shorttext synchronized">Executes the defined workflow</p>
  "! @parameter config              | <p class="shorttext synchronized">Execution configuration</p>
  "! @parameter result              | <p class="shorttext synchronized">Execution summary</p>
  "! @raising   zcx_llm_agent_error | <p class="shorttext synchronized">Execution error</p>
  METHODS execute_workflow
    IMPORTING config        TYPE configuration OPTIONAL
    RETURNING VALUE(result) TYPE execution_summary
    RAISING   zcx_llm_agent_error.

  "! <p class="shorttext synchronized">Gets current execution status</p>
  "! @parameter result | <p class="shorttext synchronized">Current execution summary</p>
  METHODS get_status
    RETURNING VALUE(result) TYPE execution_summary.

  "! <p class="shorttext synchronized">Stops workflow execution</p>
  "! @raising zcx_llm_agent_error | <p class="shorttext synchronized">Stop error</p>
  METHODS stop_workflow
    RAISING zcx_llm_agent_error.

  "! <p class="shorttext synchronized">Retrieves execution history</p>
  "! @parameter result | <p class="shorttext synchronized">Combined memory entries from all agents</p>
  METHODS get_execution_history
    RETURNING VALUE(result) TYPE zif_llm_agent=>memory_entries.

ENDINTERFACE.
