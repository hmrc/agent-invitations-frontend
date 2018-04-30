# agent-invitations-frontend

[![Build Status](https://travis-ci.org/hmrc/agent-invitations-frontend.svg)](https://travis-ci.org/hmrc/agent-invitations-frontend) [ ![Download](https://api.bintray.com/packages/hmrc/releases/agent-invitations-frontend/images/download.svg) ](https://bintray.com/hmrc/releases/agent-invitations-frontend/_latestVersion)

This is a frontend microservice for Agent Client Authorisation.
 
Invitations service currently provides these functions:
 - Agent can create an invitation to represent a client for specific tax regime.
 - Client can accept or decline agent's invitation.

Note: Currently this service supports ITSA invitations only.

## Features


## Running Tests
```
    sbt test it:test
```
 

## Running the app locally requires a number of services using this profile

```
    sm --start AGENT_MTD -f
```

or to run by itself
```
    sbt run
```

It should run at port 9448

## Stop local upstream services

```
sm --stop AGENT_MTD
```


## Endpoints
All Endpoints require Authentication.

### For Agents

Start Page for Agents:
```
GET   	/invitations/agents/
```

Fast Track Invitation API:
API to create a fast-track invitation.

```
POST   /invitations/agents/fast-track
```

The following are the supported services and relevant mandatory fields required to create a fast-track invitation:

|service|clientIdentifierType|clientIdentifier|postcode|vatRegDate|
|--------|---------|-------|-------|-------|
|HMRC-MTD-IT|ni|Valid Nino|Valid code|N/A|
|PERSONAL-INCOME-RECORD|ni|Valid Nino|N/A|N/A|
|HMRC-MTD-VAT|vrn|Valid Vat Registration Number|N/A|Date of Client's VAT Registration|

Note: clientIdentifierType is optional and will be provided by the app.

### For Clients

Start Page for Clients:
```
GET     /invitations/{invitationId}
```
### License

This code is open source software licensed under the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html")
