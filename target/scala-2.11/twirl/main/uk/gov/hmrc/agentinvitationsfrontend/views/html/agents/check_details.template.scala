
package uk.gov.hmrc.agentinvitationsfrontend.views.html.agents

import play.twirl.api._
import play.twirl.api.TemplateMagic._


     object check_details_Scope0 {
import models._
import controllers._
import play.api.i18n._
import views.html._
import play.api.templates.PlayMagic._
import play.api.mvc._
import play.api.data._

     object check_details_Scope1 {
import play.api.Configuration
import uk.gov.hmrc.agentinvitationsfrontend.controllers.routes
import uk.gov.hmrc.play.views.html.helpers.{errorSummary, form, inputRadioGroup}
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.views.html._
import scala.util.Try
import uk.gov.hmrc.agentinvitationsfrontend.views.html._
import uk.gov.hmrc.agentinvitationsfrontend.controllers.ConfirmForm
import uk.gov.hmrc.agentinvitationsfrontend.models.CurrentAuthorisationRequest
import org.joda.time.LocalDate
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentinvitationsfrontend.views.agents.CheckDetailsPageConfig

class check_details extends BaseScalaTemplate[play.twirl.api.HtmlFormat.Appendable,Format[play.twirl.api.HtmlFormat.Appendable]](play.twirl.api.HtmlFormat) with play.twirl.api.Template9[Form[ConfirmForm],CurrentAuthorisationRequest,FeatureFlags,String,CheckDetailsPageConfig,Request[_$1] forSome { 
   type _$1 >: _root_.scala.Nothing <: _root_.scala.Any
},Messages,Configuration,ExternalUrls,play.twirl.api.HtmlFormat.Appendable] {

  /**/
  def apply/*34.2*/(confirmForm: Form[ConfirmForm], currentInvitationInput: CurrentAuthorisationRequest, featureFlags: FeatureFlags, serviceMessageKey: String, pageConfig: CheckDetailsPageConfig)(implicit request: Request[_], messages: Messages, configuration: Configuration, externalUrls: ExternalUrls):play.twirl.api.HtmlFormat.Appendable = {
    _display_ {
      {

def /*36.2*/postcodeRegex/*36.15*/ = {{"^[A-Z]{1,2}[0-9][0-9A-Z]?\\s?[0-9][A-Z]{2}$|BFPO\\s?[0-9]{1,5}$"}};def /*38.2*/displayDate/*38.13*/(date: String) = {{
    val localDate = Try(LocalDate.parse(date))
    localDate.map(_.toString("dd MMMM yyyy", messages.lang.locale)).getOrElse(date)
}};def /*44.2*/postcodeSpace/*44.15*/ = {{
    if(currentInvitationInput.knownFact.get.replaceAll(" ", "").length == 6) 3
    else 4
}};
Seq[Any](format.raw/*34.286*/("""

"""),format.raw/*36.86*/("""

"""),format.raw/*41.2*/("""


"""),format.raw/*47.2*/("""

"""),_display_(/*49.2*/uk/*49.4*/.gov.hmrc.agentinvitationsfrontend.views.html.main_template(title = error_prefix(confirmForm) + Messages("generic.title", Messages("check-details.heading"), Messages("title.suffix.agents")), bodyClasses = None, isAgent = true, mainDataAttributes = Some(gaPageEvent(Messages(s"check-details.heading"), serviceMessageKey)))/*49.325*/ {_display_(Seq[Any](format.raw/*49.327*/("""

    """),_display_(/*51.6*/back_link_referer(request)),format.raw/*51.32*/("""

    """),_display_(/*53.6*/if(confirmForm.hasErrors)/*53.31*/ {_display_(Seq[Any](format.raw/*53.33*/("""
        """),_display_(/*54.10*/errorSummary(Messages("error.summary.heading"), confirmForm, Seq.empty, Some("checkDetails"))),format.raw/*54.103*/("""
    """)))}),format.raw/*55.6*/("""

    """),format.raw/*57.5*/("""<h1 class="heading-xlarge margin-bottom-30">"""),_display_(/*57.50*/Messages("check-details.heading")),format.raw/*57.83*/("""</h1>

    <p class="panel panel-border-wide">"""),_display_(/*59.41*/Messages(s"check-details.p.${currentInvitationInput.service}")),format.raw/*59.103*/("""</p>

    <h2 class="heading-medium margin-top-40">"""),_display_(/*61.47*/Messages("check-details.table.heading")),format.raw/*61.86*/("""</h2>

    <dl class="govuk-check-your-answers cya-questions-long margin-bottom-60">
        <div>
            """),_display_(/*65.14*/if(currentInvitationInput.clientType.nonEmpty)/*65.60*/{_display_(Seq[Any](format.raw/*65.61*/("""
            """),format.raw/*66.13*/("""<dt class="cya-question">
                """),_display_(/*67.18*/Messages("check-details.client-type")),format.raw/*67.55*/("""
            """),format.raw/*68.13*/("""</dt>
            <dd class="cya-answer" id="client-type">
                """),_display_(/*70.18*/{currentInvitationInput.clientType match {
                    case Some("personal") => Messages("check-details.client-type.personal")
                    case Some("business") => Messages("check-details.client-type.business")
                    case _ => throw new IllegalArgumentException("client type not found")
                }}),format.raw/*74.19*/("""
            """),format.raw/*75.13*/("""</dd>
            """)))}),format.raw/*76.14*/("""
        """),format.raw/*77.9*/("""</div>
        <div>
            <dt class="cya-question">
                """),_display_(/*80.18*/{currentInvitationInput.clientIdentifierType match {
                    case "ni" => Messages("check-details.nino")
                    case "vrn" => Messages("check-details.vrn")
                    case _ => throw new IllegalArgumentException("client identifier type not supported")
                }}),format.raw/*84.19*/("""
            """),format.raw/*85.13*/("""</dt>
            <dd class="cya-answer" id="client-identifier">
                """),_display_(/*87.18*/if(currentInvitationInput.service == "HMRC-MTD-VAT")/*87.70*/ {_display_(Seq[Any](format.raw/*87.72*/("""
                    """),_display_(/*88.22*/currentInvitationInput/*88.44*/.clientIdentifier),format.raw/*88.61*/("""
                """)))}/*89.18*/else/*89.22*/{_display_(Seq[Any](format.raw/*89.23*/("""
                    """),_display_(/*90.22*/currentInvitationInput/*90.44*/.clientIdentifier.replaceAll(" ", "").replaceAll("(.{2})", "$1 ")),format.raw/*90.109*/("""
                """)))}),format.raw/*91.18*/("""
            """),format.raw/*92.13*/("""</dd>
        </div>
        """),_display_(/*94.10*/if(pageConfig.showKnownFact)/*94.38*/ {_display_(Seq[Any](format.raw/*94.40*/("""
            """),format.raw/*95.13*/("""<div>
                <dt class="cya-question">
                """),_display_(/*97.18*/{
                    currentInvitationInput.service match {
                        case "HMRC-MTD-IT" => Messages("check-details.postcode")
                        case "PERSONAL-INCOME-RECORD" => Messages("check-details.dob")
                        case "HMRC-MTD-VAT" => Messages("check-details.vat-reg-date")
                    }
                }),format.raw/*103.18*/("""
                """),format.raw/*104.17*/("""</dt>
                <dd class="cya-answer" id="known-fact">
                """),_display_(/*106.18*/if(currentInvitationInput.service == "HMRC-MTD-IT")/*106.69*/ {_display_(Seq[Any](format.raw/*106.71*/("""
                    """),_display_(/*107.22*/if(currentInvitationInput.knownFact.get.matches(postcodeRegex))/*107.85*/ {_display_(Seq[Any](format.raw/*107.87*/("""
                        """),_display_(/*108.26*/currentInvitationInput/*108.48*/.knownFact.get.replaceAll(" ", "").replaceAll(s"(.{$postcodeSpace})", "$1 ")),format.raw/*108.124*/("""
                    """)))}/*109.22*/else/*109.26*/{_display_(Seq[Any](format.raw/*109.27*/("""
                        """),_display_(/*110.26*/currentInvitationInput/*110.48*/.knownFact.get),format.raw/*110.62*/("""
                    """)))}),format.raw/*111.22*/("""
                """)))}/*112.19*/else/*112.24*/{_display_(Seq[Any](format.raw/*112.25*/("""
                    """),_display_(/*113.22*/displayDate(currentInvitationInput.knownFact.get)),format.raw/*113.71*/("""
                """)))}),format.raw/*114.18*/("""
                """),format.raw/*115.17*/("""</dd>
            </div>
        """)))}),format.raw/*117.10*/("""
    """),format.raw/*118.5*/("""</dl>

    """),_display_(/*120.6*/if(pageConfig.needClientType)/*120.35*/ {_display_(Seq[Any](format.raw/*120.37*/("""

        """),format.raw/*122.9*/("""<div class="margin-top-40 margin-bottom-40"><a href=""""),_display_(/*122.63*/pageConfig/*122.73*/.changeDetailsUrl),format.raw/*122.90*/("""">"""),_display_(/*122.93*/Messages("check-details.change.link")),format.raw/*122.130*/("""</a></div>

        <p>"""),_display_(/*124.13*/Messages("check-details.change.p1")),format.raw/*124.48*/("""</p>

        <a href=""""),_display_(/*126.19*/pageConfig/*126.29*/.clientTypeUrl),format.raw/*126.43*/("""" class="button" id="continueMoreInfo" role="button">"""),_display_(/*126.97*/Messages("continue.button")),format.raw/*126.124*/("""</a>
    """)))}/*127.6*/else/*127.11*/{_display_(Seq[Any](format.raw/*127.12*/("""
        """),_display_(/*128.10*/if(pageConfig.needKnownFact)/*128.38*/ {_display_(Seq[Any](format.raw/*128.40*/("""
            """),format.raw/*129.13*/("""<div class="margin-top-40 margin-bottom-40"><a href=""""),_display_(/*129.67*/pageConfig/*129.77*/.changeDetailsUrl),format.raw/*129.94*/("""">"""),_display_(/*129.97*/Messages("check-details.change.link")),format.raw/*129.134*/("""</a></div>

            <p>"""),_display_(/*131.17*/Messages("check-details.change.p1")),format.raw/*131.52*/("""</p>

            <a href=""""),_display_(/*133.23*/pageConfig/*133.33*/.knownFactUrl),format.raw/*133.46*/("""" class="button" id="continueMoreInfo" role="button">"""),_display_(/*133.100*/Messages("continue.button")),format.raw/*133.127*/("""</a>
        """)))}/*134.11*/else/*134.16*/{_display_(Seq[Any](format.raw/*134.17*/("""

            """),_display_(/*136.14*/form(
                routes.AgentsInvitationController.submitDetails(),
                'class -> "margin-top-30"
            )/*139.14*/ {_display_(Seq[Any](format.raw/*139.16*/("""

                """),_display_(/*141.18*/inputRadioGroup(
                    confirmForm("checkDetails"),
                    Seq(("true", Messages("confirm-details.radio1")), ("false", Messages("confirm-details.radio2"))),
                    '_legend -> Html("""<h1 class="heading-medium flush--top">""" + Messages("confirm-details.sub-header") + """</h1>"""),
                    '_groupDivClass -> "form-group soft--ends",
                    '_fieldsetAttributes -> Html("""id='checkDetails'""")
                )),format.raw/*147.18*/("""

                """),format.raw/*149.17*/("""<button class="button" type="submit" id="continueMoreInfo">"""),_display_(/*149.77*/Messages("continue.button")),format.raw/*149.104*/("""</button>
            """)))}),format.raw/*150.14*/("""
        """)))}),format.raw/*151.10*/("""
    """)))}),format.raw/*152.6*/("""


""")))}),format.raw/*155.2*/("""
"""))
      }
    }
  }

  def render(confirmForm:Form[ConfirmForm],currentInvitationInput:CurrentAuthorisationRequest,featureFlags:FeatureFlags,serviceMessageKey:String,pageConfig:CheckDetailsPageConfig,request:Request[_$1] forSome { 
   type _$1 >: _root_.scala.Nothing <: _root_.scala.Any
},messages:Messages,configuration:Configuration,externalUrls:ExternalUrls): play.twirl.api.HtmlFormat.Appendable = apply(confirmForm,currentInvitationInput,featureFlags,serviceMessageKey,pageConfig)(request,messages,configuration,externalUrls)

  def f:((Form[ConfirmForm],CurrentAuthorisationRequest,FeatureFlags,String,CheckDetailsPageConfig) => (Request[_$1] forSome { 
   type _$1 >: _root_.scala.Nothing <: _root_.scala.Any
},Messages,Configuration,ExternalUrls) => play.twirl.api.HtmlFormat.Appendable) = (confirmForm,currentInvitationInput,featureFlags,serviceMessageKey,pageConfig) => (request,messages,configuration,externalUrls) => apply(confirmForm,currentInvitationInput,featureFlags,serviceMessageKey,pageConfig)(request,messages,configuration,externalUrls)

  def ref: this.type = this

}


}
}

/**/
object check_details extends check_details_Scope0.check_details_Scope1.check_details
              /*
                  -- GENERATED --
                  DATE: Wed Dec 05 17:06:20 GMT 2018
                  SOURCE: /Users/arturopala/workspace/hmrc/agent-invitations-frontend/app/uk/gov/hmrc/agentinvitationsfrontend/views/agents/check_details.scala.html
                  HASH: 788221a7ab50cbe84b4cdcf6285a01bb9b88adfa
                  MATRIX: 1525->1324|1888->1611|1910->1624|1995->1698|2015->1709|2181->1865|2203->1878|2332->1608|2362->1695|2391->1861|2421->1975|2450->1978|2460->1980|2791->2301|2832->2303|2865->2310|2912->2336|2945->2343|2979->2368|3019->2370|3056->2380|3171->2473|3207->2479|3240->2485|3312->2530|3366->2563|3440->2610|3524->2672|3603->2724|3663->2763|3802->2875|3857->2921|3896->2922|3937->2935|4007->2978|4065->3015|4106->3028|4209->3104|4565->3439|4606->3452|4656->3471|4692->3480|4795->3556|5120->3860|5161->3873|5270->3955|5331->4007|5371->4009|5420->4031|5451->4053|5489->4070|5526->4088|5539->4092|5578->4093|5627->4115|5658->4137|5745->4202|5794->4220|5835->4233|5892->4263|5929->4291|5969->4293|6010->4306|6102->4371|6478->4725|6524->4742|6631->4821|6692->4872|6733->4874|6783->4896|6856->4959|6897->4961|6951->4987|6983->5009|7082->5085|7124->5107|7138->5111|7178->5112|7232->5138|7264->5160|7300->5174|7354->5196|7392->5215|7406->5220|7446->5221|7496->5243|7567->5292|7617->5310|7663->5327|7729->5361|7762->5366|7801->5378|7840->5407|7881->5409|7919->5419|8001->5473|8021->5483|8060->5500|8091->5503|8151->5540|8203->5564|8260->5599|8312->5623|8332->5633|8368->5647|8450->5701|8500->5728|8529->5738|8543->5743|8583->5744|8621->5754|8659->5782|8700->5784|8742->5797|8824->5851|8844->5861|8883->5878|8914->5881|8974->5918|9030->5946|9087->5981|9143->6009|9163->6019|9198->6032|9281->6086|9331->6113|9365->6128|9379->6133|9419->6134|9462->6149|9600->6277|9641->6279|9688->6298|10188->6776|10235->6794|10323->6854|10373->6881|10428->6904|10470->6914|10507->6920|10542->6924
                  LINES: 36->34|40->36|40->36|40->38|40->38|43->44|43->44|47->34|49->36|51->41|54->47|56->49|56->49|56->49|56->49|58->51|58->51|60->53|60->53|60->53|61->54|61->54|62->55|64->57|64->57|64->57|66->59|66->59|68->61|68->61|72->65|72->65|72->65|73->66|74->67|74->67|75->68|77->70|81->74|82->75|83->76|84->77|87->80|91->84|92->85|94->87|94->87|94->87|95->88|95->88|95->88|96->89|96->89|96->89|97->90|97->90|97->90|98->91|99->92|101->94|101->94|101->94|102->95|104->97|110->103|111->104|113->106|113->106|113->106|114->107|114->107|114->107|115->108|115->108|115->108|116->109|116->109|116->109|117->110|117->110|117->110|118->111|119->112|119->112|119->112|120->113|120->113|121->114|122->115|124->117|125->118|127->120|127->120|127->120|129->122|129->122|129->122|129->122|129->122|129->122|131->124|131->124|133->126|133->126|133->126|133->126|133->126|134->127|134->127|134->127|135->128|135->128|135->128|136->129|136->129|136->129|136->129|136->129|136->129|138->131|138->131|140->133|140->133|140->133|140->133|140->133|141->134|141->134|141->134|143->136|146->139|146->139|148->141|154->147|156->149|156->149|156->149|157->150|158->151|159->152|162->155
                  -- GENERATED --
              */
          