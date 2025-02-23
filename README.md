# ABAP LLM Client

[Discord](https://discord.gg/jf2FhnFvSn) - I will post regular updates and plans there.

## Documentation

See [Overview](docs/Overview.md) in docs folder.

## Overview

ABAP LLM Client provides a universal strongly opinionated interface to on-premise and cloud-based LLM APIs, focusing primarily on chat endpoints.
The client is designed to seamlessly integrate with various LLM providers while maintaining a consistent interface for ABAP applications.

Currently supported providers:

- OLLAMA
- OpenAI
- OpenRouter
- Azure OpenAI
- Azure AI Foundry
- Anthropic (structured output is not supported, considering simulating this via tool calls - vote in issue for that)
- Google Vertex AI
- Deepseek (structured output currently not supported)
- Google Gemini API
- AWS Bedrock via Converse API Endpoint

## Features

- Chat messages
- Structured output
- Tool (function) call
- Multiple provider integration
- Customizable

## Known Limitations

- Tested only on 7.52 & ABAP 2022 - **experimental** downport available in branch 702
- No advanced parsing to compensate LLM output errors especially in structured output and tool calls

## Not planned

The following topics are currently not considered but feel free to challenge that in discussions if you have a specific use case.

- Port to ABAP Cloud - if you use BTP you might aswell consider using Python or other languages with better GenAI library support
- Non-Chat (e.g. image, audio, video) support

## Why ABAP LLM Client?

ABAP might not be the first environment that comes to mind for GenAI projects, but it's the foundation for critical business processes in many organizations. This client exists to bridge this gap:
It provides a straightforward way to integrate LLM capabilities into existing ABAP business logic.

### Core Ideas

- **Simple Integration**: Focus on the most commonly needed LLM features (mainly chat endpoints) rather than trying to support every possible API feature
- **Practical Approach**: Built for ABAP developers who need to enhance existing business functions with LLM capabilities, not for ML experiments
- **Stable Integration**: As tools and structured output become standard features across LLM providers, integration becomes more reliable and maintainable
- **Real-World Usage**: Designed for actual business scenarios where you might want to:
  - Enhance existing ABAP reports with AI-powered insights
  - Add LLM capabilities to business workflows
  - Prototype AI features in existing ABAP applications

### No Frills

This is not a full-featured ML framework or a complex AI platform. It's a practical tool for ABAP developers who need to call LLM APIs and process their responses within their ABAP environment.
Nothing more, nothing less. If you need to integrate LLM capabilities into your ABAP systems and want to do it with minimal overhead, this client might be useful for you.

## Usage

Refer to the [llm_client_test](https://github.com/abap-ai/llm_client_tests) repository on how to use the llm client and the docs folder for further details.

⚠️ **Security Notice**: Currently no safeguards. Use API keys with appropriate usage limits to prevent accidental charges e.g. for unexpected endless loops, etc.

## Extension

Custom LLM Authenticaton implementations can be created by:

1. Sub-classing the specific provider class
2. Redefining the `SET_AUTH` method
In general for most parts interfaces are available to easily use custom implementations preferrably via sub-classes.

## Roadmap - Planned Features

With the Beta release my focus is on using this library for further AI based topics (stay tuned) --> bug fixes have priority. Open a discussion or an issue if you have a specific requirement.
Further features mid-term (check discord for short term plans and to discuss the current state):

- Workaround to force use required tool to simulate structured output where providers miss this feature (e.g. Azure AI Foundry)
- Better error- and edge-case handling
- Optimizations:
  - Code re-use for the clients where useful
  - Consider a common JSON-Schema builder for structured output and tool use
  - Performance Optimizations (low prio)
- ??? - Open issues/discussions for whatever comes to your mind

## Contributing

Feel free to:

- Open issues for bug reports
- Use [Discord](https://discord.gg/jf2FhnFvSn) for discussions
- Request/Implement additional provider implementations
- Suggest concept improvements

## Security Recommendations

- Use SM59 supported authentication methods e.g. by using an API Gateway for credential mapping
- Set appropriate usage limits on API keys and monitor their usage
- Implement custom authorization checks to limit who can use the LLM Client
