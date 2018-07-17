# agent-invitations-frontend

[![Build Status](https://travis-ci.org/hmrc/agent-invitations-frontend.svg)](https://travis-ci.org/hmrc/agent-invitations-frontend) [ ![Download](https://api.bintray.com/packages/hmrc/releases/agent-invitations-frontend/images/download.svg) ](https://bintray.com/hmrc/releases/agent-invitations-frontend/_latestVersion)

## What the service does
This is a frontend microservice for Agent Client Authorisation.

Invitations service currently provides these functions:
 - Agent can create an invitation to represent a client for specific tax regime
 - Client can accept or decline agent's invitation

Currently this service supports ITSA, PIR and VAT invitations.

Feature flags exist for each service and for requirement of known facts.

## Features

### Running the tests

    sbt test it:test

### Running the tests with coverage

    sbt clean coverageOn test it:test coverageReport

### Running the app locally

    sm --start AGENT_MTD -f
    sm --stop AGENT_INVITATIONS_FRONTEND
    sbt run

It should then be listening on port 9448

    browse http://localhost:9448/invitations/agents

## Endpoints
All Endpoints require Authentication.

### For Agents

Start Page for Agents:

    GET   	/invitations/agents/

Fast Track Invitation:

API to create a fast-track invitation.

```
POST   /invitations/agents/fast-track
```

The following are the supported services and relevant mandatory fields required to create a fast-track invitation:

|service|clientIdentifierType|clientIdentifier|knownFact|
|--------|---------|-------|-------|
|HMRC-MTD-IT|ni|Valid Nino|Valid Postcode|
|PERSONAL-INCOME-RECORD|ni|Valid Nino|Date of Birth|
|HMRC-MTD-VAT|vrn|Valid Vat Registration Number|Date of Client's VAT Registration|

Note: If any information is missing / invalid / unsupported, you will be redirected to the appropriate page to fill in.

### For Clients

Start Page for Clients:

    GET     /invitations/{invitationId}

### License 

This code is open source software licensed under the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html")
