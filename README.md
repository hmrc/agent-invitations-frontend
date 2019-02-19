# agent-invitations-frontend

[![Build Status](https://travis-ci.org/hmrc/agent-invitations-frontend.svg)](https://travis-ci.org/hmrc/agent-invitations-frontend) [ ![Download](https://api.bintray.com/packages/hmrc/releases/agent-invitations-frontend/images/download.svg) ](https://bintray.com/hmrc/releases/agent-invitations-frontend/_latestVersion)

## What the service does
This is a frontend microservice for Agent Client Authorisation.

Invitations service currently provides these functions:
 - Agent can create an invitation to represent a client for specific tax regime
 - Client can accept or decline an agent's invitation

Currently this service supports ITSA, PIR and VAT invitations.

Feature flags exist for each service and for requirement of known facts.

## Features

### Running the tests

    sbt test it:test

### Running the tests with coverage

    sbt clean coverageOn test it:test coverageReport

### Running the app locally

    sm --start AGENT_AUTHORISATIONS -f
    sm --stop AGENT_INVITATIONS_FRONTEND
    sbt run

It should then be listening on port 9448

    browse http://localhost:9448/invitations/agents

## Endpoints
All Endpoints require Authentication.

### For Agents

Start page for Agents:

    GET   	/invitations/agents/

Fast Track Invitation:

API to create a fast-track invitation.

```
POST   /invitations/agents/fast-track
```

The following are the supported services and relevant fields required to create a fast track invitation:

|clientType|service|clientIdentifierType|clientIdentifier|knownFact|
|--------|--------|---------|-------|-------|
|personal|HMRC-MTD-IT|ni|Valid Nino|Postcode|
|personal|PERSONAL-INCOME-RECORD|ni|Valid Nino|Date of Birth|
|personal or business|HMRC-MTD-VAT|vrn|Valid Vat Registration Number|Date of Client's VAT Registration|

Note: Client Type and Known Fact are optional. If either of those are missing you will be redirected to the appropriate page. However, if any other information is missing / invalid / unsupported, you will be given an error url.

### For Clients

Start Page for Clients (Old):

Note: This entry point will not be used until TBC


    GET     /invitations/{invitationId}
    
    
Start Page for Clients (New):

Note: Replaces old entry point. Currently In-progress.

```
    GET     /invitations/{clientType}/{uid}/{agentName}

```

### License 

This code is open source software licensed under the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html")
