# agent-invitations-frontend

[![Build Status](https://travis-ci.org/hmrc/agent-invitations-frontend.svg)](https://travis-ci.org/hmrc/agent-invitations-frontend) [ ![Download](https://api.bintray.com/packages/hmrc/releases/agent-invitations-frontend/images/download.svg) ](https://bintray.com/hmrc/releases/agent-invitations-frontend/_latestVersion)

This is a frontend microservice for Agent Client Authorisation and the replacement for Agent Client Authorisation Frontend.
 
Invitations service currently provides these functions:
 - Allow agents to act on behalf of clients.
 - Allow clients to accept or decline agents.

Note: Currently this service supports ITSA.

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

### For Clients

Start Page for Clients:
```
GET     /invitations/{invitationId}
```
### License

This code is open source software licensed under the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html")