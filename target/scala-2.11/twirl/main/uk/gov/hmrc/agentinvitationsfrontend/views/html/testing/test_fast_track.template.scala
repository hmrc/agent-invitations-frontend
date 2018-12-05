
package uk.gov.hmrc.agentinvitationsfrontend.views.html.testing

import play.twirl.api._
import play.twirl.api.TemplateMagic._


     object test_fast_track_Scope0 {
import models._
import controllers._
import play.api.i18n._
import views.html._
import play.api.templates.PlayMagic._
import play.api.mvc._
import play.api.data._

     object test_fast_track_Scope1 {
import play.api.Configuration
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{routes => agentRoutes}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.testing.{routes => testRoutes}
import uk.gov.hmrc.agentinvitationsfrontend.models.CurrentAuthorisationRequest
import uk.gov.hmrc.play.views.html.helpers.{form, input}
import uk.gov.hmrc.agentinvitationsfrontend.support.CallOps._

class test_fast_track extends BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with play.twirl.api.Template6[Form[CurrentAuthorisationRequest],Boolean,Request[_$1] forSome { 
   type _$1 >: _root_.scala.Nothing <: _root_.scala.Any
},Configuration,Messages,ExternalUrls,play.twirl.api.HtmlFormat.Appendable] {

  /**/
  def apply/*24.2*/(fastTrackForm: Form[CurrentAuthorisationRequest], isDev: Boolean)(implicit request: Request[_], configuration: Configuration, messages: Messages, externalUrls: ExternalUrls):play.twirl.api.HtmlFormat.Appendable = {
    _display_ {
      {

def /*26.2*/postToInvitations/*26.19*/ = {{
    val baseUrl = if(isDev) s"http://${request.host}" else ""
    addParamsToUrl(s"${agentRoutes.AgentsInvitationController.agentFastTrack().url}",
        "continue" -> Some(s"$baseUrl${testRoutes.TestEndpointsController.getFastTrackForm()}"),
        "error" -> Some(s"$baseUrl${testRoutes.TestEndpointsController.getFastTrackForm()}")
    )
}};
Seq[Any](format.raw/*24.176*/("""

"""),format.raw/*32.2*/("""


"""),_display_(/*35.2*/uk/*35.4*/.gov.hmrc.agentinvitationsfrontend.views.html.main_template(title = "Test Only: Fast Track Invitation", isAgent = true)/*35.123*/ {_display_(Seq[Any](format.raw/*35.125*/("""
  """),format.raw/*36.3*/("""<h1>Test Only: Fast Track Invitation</h1>

  """),_display_(/*38.4*/form(action = Call("POST", postToInvitations))/*38.50*/ {_display_(Seq[Any](format.raw/*38.52*/("""
    """),format.raw/*39.5*/("""<fieldset class="form-field-group">
      <div class="form-group">
        """),_display_(/*41.10*/input(
          fastTrackForm("clientType"),
          '_label -> "Enter Client Type",
          '_inputClass -> "input--medium input--cleared")),format.raw/*44.58*/("""

        """),_display_(/*46.10*/input(
          fastTrackForm("service"),
          '_label -> "Enter Service",
          '_inputClass -> "input--medium input--cleared"
        )),format.raw/*50.10*/("""
        """),_display_(/*51.10*/input(
          fastTrackForm("clientIdentifier"),
          '_label -> "Enter ClientId",
          '_inputClass -> "input--medium input--cleared"
        )),format.raw/*55.10*/("""

        """),_display_(/*57.10*/input(
          fastTrackForm("clientIdentifierType"),
          '_label -> "Enter ClientIdType",
          '_inputClass -> "input--medium input--cleared"
        )),format.raw/*61.10*/("""

          """),_display_(/*63.12*/input(
          fastTrackForm("knownFact"),
          '_label -> "Enter Known Fact",
          '_inputClass -> "input--medium input--cleared"
        )),format.raw/*67.10*/("""

          """),_display_(/*69.12*/Html("""<p>""" + "Client Types: personal / business" + """</p>""")),format.raw/*69.78*/("""

          """),_display_(/*71.12*/Html("""<p>""" + "Client Id Types: ni / vrn" + """</p>""")),format.raw/*71.70*/("""

          """),_display_(/*73.12*/Html("""<p>""" + "Example Postcode: DH14EJ" + """</p>""" + """<p>""" + "Example Date: 2007-07-07" + """</p>""")),format.raw/*73.123*/("""
      """),format.raw/*74.7*/("""</div>
    </fieldset>
    <div class="form-group">
      <button class="button" type="submit" id="continue">Create</button>
    </div>
  """)))}),format.raw/*79.4*/("""
""")))}))
      }
    }
  }

  def render(fastTrackForm:Form[CurrentAuthorisationRequest],isDev:Boolean,request:Request[_$1] forSome { 
   type _$1 >: _root_.scala.Nothing <: _root_.scala.Any
},configuration:Configuration,messages:Messages,externalUrls:ExternalUrls): play.twirl.api.HtmlFormat.Appendable = apply(fastTrackForm,isDev)(request,configuration,messages,externalUrls)

  def f:((Form[CurrentAuthorisationRequest],Boolean) => (Request[_$1] forSome { 
   type _$1 >: _root_.scala.Nothing <: _root_.scala.Any
},Configuration,Messages,ExternalUrls) => play.twirl.api.HtmlFormat.Appendable) = (fastTrackForm,isDev) => (request,configuration,messages,externalUrls) => apply(fastTrackForm,isDev)(request,configuration,messages,externalUrls)

  def ref: this.type = this

}


}
}

/**/
object test_fast_track extends test_fast_track_Scope0.test_fast_track_Scope1.test_fast_track
              /*
                  -- GENERATED --
                  DATE: Wed Dec 05 17:06:20 GMT 2018
                  SOURCE: /Users/arturopala/workspace/hmrc/agent-invitations-frontend/app/uk/gov/hmrc/agentinvitationsfrontend/views/testing/test_fast_track.scala.html
                  HASH: 8f956891203c53eb1e5bab8610349ba073909dab
                  MATRIX: 1243->1072|1496->1249|1522->1266|1905->1246|1934->1617|1964->1621|1974->1623|2103->1742|2144->1744|2174->1747|2246->1793|2301->1839|2341->1841|2373->1846|2476->1922|2642->2067|2680->2078|2848->2225|2885->2235|3063->2392|3101->2403|3287->2568|3327->2581|3500->2733|3540->2746|3627->2812|3667->2825|3746->2883|3786->2896|3919->3007|3953->3014|4122->3153
                  LINES: 31->24|35->26|35->26|42->24|44->32|47->35|47->35|47->35|47->35|48->36|50->38|50->38|50->38|51->39|53->41|56->44|58->46|62->50|63->51|67->55|69->57|73->61|75->63|79->67|81->69|81->69|83->71|83->71|85->73|85->73|86->74|91->79
                  -- GENERATED --
              */
          