package forms
import play.api.data.FormError
import play.api.libs.json.Json
import uk.gov.hmrc.agentinvitationsfrontend.controllers.ClientsMultiInvitationController.confirmTermsMultiForm
import uk.gov.hmrc.agentinvitationsfrontend.controllers.ConfirmTermsMultiForm
import uk.gov.hmrc.agentinvitationsfrontend.models.UserInputNinoAndPostcode
import uk.gov.hmrc.play.test.UnitSpec

class ConfirmTermsMultiSpec extends UnitSpec{


//  val clientTypeEmptyMessage: String = "error.client-type.required"
//  val clientTypeEmptyFormError: FormError = FormError("clientType", List(clientTypeEmptyMessage))
//  val serviceITSA = "HMRC-MTD-IT"
//  val servicePIR = "PERSONAL-INCOME-RECORD"
//  val serviceVAT = "HMRC-MTD-VAT"
//  val personal = Some("personal")
//  val business = Some("business")

  "ConfirmTermsMultiForm" should {
//    "return no error message for valid clientType Personal" in {
//      val data = Json.obj("confirmTerms" -> Seq(true, true))
//      val clientTypeForm = confirmTermsMultiForm.bind(data)
//      clientTypeForm.errors.isEmpty shouldBe true
//    }

    "return no errors when unbinding the form" in {
      val unboundForm =
        confirmTermsMultiForm.mapping.unbind(ConfirmTermsMultiForm(Seq(true, true)))
      unboundForm("confirmTerms") shouldBe Seq(true, true)

//      val unboundFormAFI =
//        agentInvitationSelectClientTypeForm.mapping.unbind(
//          UserInputNinoAndPostcode(personal, servicePIR, Some("AE123456C"), None))
//      unboundFormAFI("clientType") shouldBe "personal"
//
//      val unboundFormVAT =
//        agentInvitationSelectClientTypeForm.mapping.unbind(
//          UserInputNinoAndPostcode(business, serviceVAT, Some("101747696"), None))
//      unboundFormVAT("clientType") shouldBe "business"
//    }

    }
  }

}
