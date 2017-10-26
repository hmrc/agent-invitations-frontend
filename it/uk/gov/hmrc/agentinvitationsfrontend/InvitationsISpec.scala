package uk.gov.hmrc.agentinvitationsfrontend

import org.scalatest.{FeatureSpecLike, GivenWhenThen}

class InvitationsISpec extends FeatureSpecLike with GivenWhenThen {


  feature("Create Invitation") {
    scenario("An Authorised Agent successfully creating an invitation") {
      Given("I am an Agent validated on MDTP")
      And("I have an activated HMRC-AS-AGENT enrolment")
      And("I have navigated to the Create Invite Page")
      And("I have entered a valid format NINO & Postcode")
      And("the NINO And Postcode provided relate to a Client that has the HMRC-MTD-IT enrolment")
      When("I press Continue")
      Then("an Invite is created")
      And("the confirmation page is displayed")
      And("the content is as per the 2.png attachment")
    }
  }

  feature("Fail Nino") {
    scenario("An Authorised Agent entered invalid Nino") {
      Given("I am an Agent validated on MDTP")
      And("I have an activated HMRC -AS - AGENT enrolment")
      And("I have navigated to the Create Invite Page")
      And("I have entered a NINO in an invalid format")
      When("I press Continue")
      Then("a validation error message is displayed")
    }
  }

  feature("Fail Postcode") {
    scenario("An Authorised Agent entered invalid Postcode") {
      Given("I am an Agent validated on MDTP")
      And("I have an activated HMRC-AS-AGENT enrolment")
      And("I have navigated to the Create Invite Page")
      And("I have entered a valid format NINO And I press Continue")
      And("I entered a Postcode in an invalid format")
      When("I press Continue")
      Then("a validation error message is displayed")
    }
  }

}
