# Provider Configuration

## [OpenAI](https://openai.com/)

Create the SM59 destination type "G" with:

- HOST api.openai.com
- Port 443
- Path Prefix /v1
- SSL Active & SSL Certificate as the usual SSL Client that has the required certificates (by default use the DFAUL SSL Client)
- Special Options HTTP 1.1

Provider configuration via Transaction ZLLM_PROVIDER_CONFIG:

- Implementation Class: ZCL_LLM_CLIENT_OPENAI
- RFC Desitination: created above
- Auth Value: Paste the API Key

## [OpenRouter](https://openrouter.ai/)

Create the SM59 destination type "G" with:

- HOST openrouter.ai
- Port 443
- Path Prefix /api/v1
- SSL Active & SSL Certificate as the usual SSL Client that has the required certificates (by default use the DFAUL SSL Client)
- Special Options HTTP 1.1

Provider configuration via Transaction ZLLM_PROVIDER_CONFIG:

- Implementation Class: ZCL_LLM_CLIENT_OPENROUTER
- RFC Desitination: created above
- Auth Value: Paste the API Key

## [Ollama](https://ollama.com/)

Create the SM59 destination type "G" with:

- HOST - your ollama host
- Port - your ollama port (default 11434)
- Path Prefix /api
- SSL - depends on your setup, by default ollama uses http
- Special Options HTTP 1.1

Provider configuration via Transaction ZLLM_PROVIDER_CONFIG:

- Implementation Class: ZCL_LLM_CLIENT_OLLAMA
- RFC Desitination: created above
- Auth Type: blank - alternative if you use some kind of reverse proxy/API Gateway that adds api-key you can use:
  - Auth Value: {apikey-header-name}:{apikey-value} Example: ApiKeyHeader:myApiKey123456

## [Azure OpenAI Service](https://azure.microsoft.com/de-de/products/ai-services/openai-service)

Create the SM59 destination type "G" with:

- HOST {deployment}.cognitiveservices.azure.com
- Port 443
- Path Prefix /openai/deployments
- SSL Active & SSL Certificate as the usual SSL Client that has the required certificates (by default use the DFAUL SSL Client)
- Special Options HTTP 1.1

Provider configuration via Transaction ZLLM_PROVIDER_CONFIG:

- Implementation Class: ZCL_LLM_CLIENT_AZUREOAI
- RFC Desitination: created above
- Auth Value: Paste the API Key

## [Azure AI Foundry](https://learn.microsoft.com/en-us/azure/ai-studio/what-is-ai-studio)

Note: Tests only happened with serverless deployments.\
Create the SM59 destination type "G" with:

- HOST {deployment}.services.ai.azure.com
- Port 443
- Path Prefix /models
- SSL Active & SSL Certificate as the usual SSL Client that has the required certificates (by default use the DFAUL SSL Client)
- Special Options HTTP 1.1

Provider configuration via Transaction ZLLM_PROVIDER_CONFIG:

- Implementation Class: ZCL_LLM_CLIENT_AZUREAIF
- RFC Desitination: created above
- Auth Value: Paste the API Key

## [Anthropic](https://docs.anthropic.com/en/home)

Create the SM59 destination type "G" with:

- HOST api.anthropic.com
- Port 443
- Path Prefix /v1
- SSL Active & SSL Certificate as the usual SSL Client that has the required certificates (by default use the DFAUL SSL Client)
- Special Options HTTP 1.1

Provider configuration via Transaction ZLLM_PROVIDER_CONFIG:

- Implementation Class: ZCL_LLM_CLIENT_ANTHROPIC
- RFC Desitination: created above
- Auth Value: Paste the API Key

## [VertexAI](https://cloud.google.com/vertex-ai/generative-ai/docs/model-reference/inference)

Configure a custon SSF Application exactly as necessary for [Google ABAP SDK](https://cloud.google.com/solutions/sap/docs/abap-sdk/on-premises-or-any-cloud/latest/authentication-jwt). As a hint you might anyhow want to install this SDK as it gives you access to a lot of great features, however it is not a prerequisite for this llm library. We just need the SSF Application and proper service account.

Create the SM59 destination type "G" for Login with:

- HOST oauth2.googleapis.com
- Port 443
- Path Prefix /token
- SSL Active & SSL Certificate as the usual SSL Client that has the required certificates (by default use the DFAUL SSL Client)
- Special Options HTTP 1.1

Create the SM59 destination type "G" for AI API calls with:

- HOST europe-west3-aiplatform.googleapis.com - or any other location as documented [here](https://cloud.google.com/vertex-ai/docs/general/locations)
- Port 443
- Path Prefix /v1/projects/{projectId}/locations - Replace the project id with your project id
- SSL Active & SSL Certificate as the usual SSL Client that has the required certificates (by default use the DFAUL SSL Client)
- Special Options HTTP 1.1

Provider configuration via Transaction ZLLM_PROVIDER_CONFIG:

- Implementation Class: ZCL_LLM_CLIENT_VERTEXAI
- RFC Desitination: created above
- Auth Value: {ssfapplication};{service account email} - for example ZG_JWT;serviceAccount\@projectId.iam.gserviceaccount.com
Hint: the semikolon ";" is mandatory between the ssf application and service account email

## [deepseek](https://www.deepseek.com/)

Create the SM59 destination type "G" with:

- HOST api.deepseek.com
- Port 443
- Path Prefix - Empty, keep the field empty
- SSL Active & SSL Certificate as the usual SSL Client that has the required certificates (by default use the DFAUL SSL Client)
- Special Options HTTP 1.1

Provider configuration via Transaction ZLLM_PROVIDER_CONFIG:

- Implementation Class: ZCL_LLM_CLIENT_DEEPSEEK
- RFC Desitination: created above
- Auth Value: Paste the API Key

## [Gemini Developer API](https://aistudio.google.com/)

Create the SM59 destination type "G" with:

- HOST generativelanguage.googleapis.com
- Port 443
- Path Prefix /v1beta/models
- SSL Active & SSL Certificate as the usual SSL Client that has the required certificates (by default use the DFAUL SSL Client)
- Special Options HTTP 1.1

Provider configuration via Transaction ZLLM_PROVIDER_CONFIG:

- Implementation Class: ZCL_LLM_CLIENT_GEMINI
- RFC Desitination: created above
- Auth Value: Paste the API Key

## [AWS Bedrock](https://aws.amazon.com/bedrock)

Create the SM59 destination type "G" with:

- HOST - use the endpoint as per [AWS Documentation](https://docs.aws.amazon.com/general/latest/gr/bedrock.html), e.g. bedrock.us-east-1.amazonaws.com
- Port 443
- Path Prefix /model
- SSL Active & SSL Certificate as the usual SSL Client that has the required certificates (by default use the DFAUL SSL Client)
- Special Options HTTP 1.1

Provider configuration via Transaction ZLLM_PROVIDER_CONFIG:

- Implementation Class: ZCL_LLM_CLIENT_AWS
- RFC Desitination: created above
- Auth Value: {AWS Credential},{host from above},{region} for example credentialxyz,secret345345asdfsa,bedrock.us-east-1.amazonaws.com,us-east-1
