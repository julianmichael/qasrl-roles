package qasrl.roles.browse

import scalacss.DevDefaults._
import scala.language.postfixOps

import jjm.ui.View

object FrameBrowserStyles extends View.Styles {
  import dsl._

  // color scheme

  val headerBackgroundColor = grey(240)
  val headerContentColor = black

  val labeledHighlightColor = rgba(  0, 128, 256, 0.1)
  val selectedHighlightColor = grey(200)
  val hoverHighlightColor = grey(240)
  val alternatingRowBackgroundColor1 = white
  val alternatingRowBackgroundColor2 = grey(240)

  val originalRoundIndicatorColor = grey(200)
  val expansionRoundIndicatorColor = rgba(  64, 192,   0, 1.0)
  val evalRoundIndicatorColor = orange
  val predictionRoundIndicatorColor = red

  val metadataLabelBackgroundColor = grey(240)

  val validTextColor = green
  val invalidTextColor = red

  val paneDivisionBorderWidth = 1 px
  val paneDivisionBorderColor = metadataLabelBackgroundColor

  val documentSelectionPaneBorder = style(
    borderLeftColor(paneDivisionBorderColor),
    borderLeftStyle.solid,
    borderLeftWidth(paneDivisionBorderWidth),
  )

  val sentenceSelectionPaneBorder = style(
    borderLeftColor(paneDivisionBorderColor),
    borderRightColor(paneDivisionBorderColor),
    borderLeftStyle.solid,
    borderRightStyle.solid,
    borderLeftWidth(paneDivisionBorderWidth),
    borderRightWidth(paneDivisionBorderWidth)
  )

  val auditingPaneBorder = style(
    borderTopColor(paneDivisionBorderColor),
    borderTopStyle.solid,
    borderTopWidth(paneDivisionBorderWidth),

  )

  // overrides

  override val checkboxSpan = style(
    addClassNames("ml-3", "pl-3")
  )

  override val checkbox = style(
    addClassNames("form-check-input")
  )

  override val checkboxLabel = style(
    addClassNames("form-check-label")
  )

  override val invalidTextBackground = style(
    backgroundColor(rgba(255, 0, 0, 0.3))
  )

  override val textField = style()

  override val shortTextField = style(
    width(50 px)
  )

  override val intArrowFieldInput = style(
    width(3 em)
  )

  // styles

  val webkitScrollbar = {
    import scalacss.internal._
    Cond(Some(Pseudo.Custom("::-webkit-scrollbar", PseudoType.Element)), Vector.empty)
  }

  val mainContainer = style(
    addClassNames("container-fluid", "p-0", "m-0")
  )

  // header

  val headerHeight = 60 px

  val flexyHeaderThing = style(
    display.flex,
    flexDirection.row,
    flexWrap.nowrap,
    position.relative,
    zIndex(10)
  )

  val headerColumn = style(
    display.flex,
    flexDirection.column,
    flexWrap.nowrap,
    position.relative
  )

  val headerContainer = style(
    addClassNames("p-2"),
    flexyHeaderThing,
    alignItems.center,
    height(headerHeight),
    backgroundColor(headerBackgroundColor),
    color(headerContentColor),
  )

  val featureOptions = style(
    flexyHeaderThing
  )

  val labeledDropdown = style(
    addClassNames("px-2")
  )

  val labeledDropdownLabel = style(
    addClassNames("pr-1")
  )

  val titleAndSearchContainer = style(
    addClassNames("pr-2")
  )

  // title

  val title = style(
    whiteSpace.nowrap,
    overflow.hidden
  )

  // search

  val searchContainer = style(
    flexyHeaderThing,
    width(100 %%)
  )

  val searchInput = style(
    flex := "1"
  )
  val searchSubmitButton = style()
  val searchClearButton = style()

  // filters

  val filterContainer = style(
    addClassNames("px-4"),
    flexyHeaderThing
  )

  val filterChooser = style(
    addClassNames("form-check", "pl-5")
  )

  val partitionChooser = style(
    filterChooser
  )

  val domainChooser = style(
    filterChooser
  )

  val sliceChooser = style(
    filterChooser
  )

  // legend

  val legendColumn = style(
    addClassNames("px-2")
  )

  val legendContainer = style(
    addClassNames("pt-1"),
    minWidth(350 px)
  )

  val legendTitle = style()

  val legendTitleText = style(
    fontWeight.bold
  )
  val legendTitleLinkText = style()

  val validityLegend = style()

  val highlightLegend = style()

  val legendColorIndicator = style()

  // help modal

  val helpModal = style(
    addClassNames("modal", "fade")
  )
  val helpModalDialog = style(
    addClassNames("modal-dialog"),
    maxWidth(800 px)
  )
  val helpModalContent = style(
    addClassNames("modal-content")
  )
  val helpModalHeader = style(
    addClassNames("modal-header")
  )
  val helpModalTitle = style(
    addClassNames("modal-title"),
    fontWeight.bold,
    fontSize(16 pt)
  )
  val helpModalHeaderCloseButton = style(
    addClassNames("close")
  )
  val helpModalBody = style(
    addClassNames("modal-body")
  )
  val helpModalFooter = style(
    addClassNames("modal-footer")
  )
  val helpModalFooterCloseButton = style(
    addClassNames("btn", "btn-secondary")
  )

  val helpWarningAlert = style(
    addClassNames("alert", "alert-warning")
  )

  // main data display

  val dataContainer = style(
    position.relative,
    overflow.hidden,
    backfaceVisibility.hidden,
    willChange := "overflow",
    display.flex,
    height(100 vh),
    marginTop(-headerHeight),
    paddingTop(headerHeight),
    width(100 %%)
  )

  val scrollPane = style(
    overflow.auto,
    height.auto,
    webkitScrollbar(
      display.none
    )
    // attr("-webkit-overflow-scrolling") := "touch",
    // attr("-ms-overflow-style") := "none"
  )

  // selection of sentences

  val metadataLabelHeight = 1 rem
  val metadataLabelFontSize = 8 pt

  val metadataLabel = style(
    display.block,
    height(metadataLabelHeight),
    backgroundColor(metadataLabelBackgroundColor),
    fontSize(metadataLabelFontSize),
    verticalAlign.middle
  )
  val metadataLabelText = style(
    addClassNames("px-1"),
    whiteSpace.nowrap
  )

  val documentSelectionPaneWidth = 10 rem
  val sentenceSelectionPaneWidth = 12 rem

  val documentSelectionFontSize = 12 pt
  val sentenceSelectionFontSize = 10 pt

  val contentPaneContainer = style(
    position.relative,
    overflow.hidden,
    backfaceVisibility.hidden,
    willChange := "overflow",
    display.flex,
    flexDirection.column
  )

  val selectionPane = style(
    scrollPane,
    lineHeight(1.2)
  )

  val countLabel = style(
    metadataLabel,
    textAlign.right
  )
  val countLabelText = style(
    metadataLabelText
  )

  val selectionEntry = style(
    addClassNames("p-2"),
    &.hover(
      backgroundColor(hoverHighlightColor)
    )
  )

  val currentSelectionEntry = style(
    selectionEntry,
    backgroundColor(selectedHighlightColor).important
  )

  val nonCurrentSelectionEntry = style(
    selectionEntry
  )

  val documentSelectionPaneContainer = style(
    contentPaneContainer,
    documentSelectionPaneBorder,
    width(documentSelectionPaneWidth),
  )

  val documentCountLabel = style(
    countLabel
  )

  val documentCountLabelText = style(
    countLabelText
  )

  val documentSelectionPane = style(
    selectionPane,
    width(100 %%)
  )

  val documentSelectionEntry = style(
    selectionEntry
  )

  val documentSelectionEntryText = style(
    fontSize(documentSelectionFontSize)
  )

  val sentenceSelectionPaneContainer = style(
    contentPaneContainer,
    sentenceSelectionPaneBorder,
    width(sentenceSelectionPaneWidth)
  )

  val sentenceCountLabel = style(
    countLabel
  )

  val sentenceCountLabelText = style(
    countLabelText
  )

  val sentenceSelectionPane = style(
    selectionPane,
  )

  val sentenceSelectionEntry = style(
    selectionEntry,
  )

  val labeledDocumentSelectionEntry = style(
    selectionEntry,
    backgroundColor(labeledHighlightColor)
  )

  val labeledSentenceSelectionEntry = style(
    selectionEntry,
    backgroundColor(labeledHighlightColor)
  )

  val sentenceSelectionEntryText = style(
    fontSize(sentenceSelectionFontSize),
  )

  // display of document biggy thing

  val documentContainer = style(
    flex := "1",
    display.flex,
    flexDirection.row,
    overflow.hidden,
    position.relative,
    backfaceVisibility.hidden,
    willChange := "overflow"
  )

  // display of sentence data

  val sentenceDisplayPane = style(
    contentPaneContainer,
    flex := "1"
  )

  val sentenceInfoContainer = style(
    addClassNames("pl-2"),
    metadataLabel,
    textAlign.left
  )
  val sentenceInfoText = style(
    metadataLabelText
  )

  val sentenceTextContainer = style(
    addClassNames("p-3"),
  )

  val verbAnchorLink = style(
    &.hover(
      textDecoration := "none"
    )
  )

  val verbEntriesContainer = style(
    scrollPane,
    flex := "1"
  )

  val loadingNotice = style(
    addClassNames("p-3")
  )

  val sentenceText = style(
    fontSize(16 pt)
  )

  val verbEntryDisplay = style(
    addClassNames("px-4", "pb-4"),
    width(100 %%)
  )

  val verbHeading = style()

  val verbHeadingText = style(
    fontSize(16 pt),
    fontWeight.bold
  )

  val verbQAsTable = style(
    tableLayout.fixed,
    width(100 %%)
  )

  val verbQAsTableBody = style(
    width(100 %%)
  )

  val hoverHighlightedVerbTable = style(
    // backgroundColor(hoverHighlightColor)
  )

  val highlightedFrame = style(
    backgroundColor(hoverHighlightColor)
  )

  val argFirstRow = style(
    addClassNames("pt-2")
  )

  val roleDisplay = style(
    addClassNames("pb-2")
  )

  val qaPairRow = style(
    addClassNames("p-1"),
    width(100 %%),
    // height(2 em)
    // &.nthChild("odd")(
    //   backgroundColor(alternatingRowBackgroundColor1)
    // ),
    // &.nthChild("even")(
    //   backgroundColor(alternatingRowBackgroundColor2)
    // )
  )

  val roundIndicator = style(
    width(0.2 rem),
    height(100 %%)
  )

  val originalRoundIndicator = style(
    roundIndicator,
    backgroundColor(originalRoundIndicatorColor)
  )

  val expansionRoundIndicator = style(
    roundIndicator,
    backgroundColor(expansionRoundIndicatorColor)
  )

  val evalRoundIndicator = style(
    roundIndicator,
    backgroundColor(evalRoundIndicatorColor)
  )

  // detour to legend real quick

  val roundLegendMark = style(
    addClassNames("ml-2"),
    display.inlineBlock,
    color.transparent
  )

  val originalLegendMark = style(
    roundLegendMark,
    originalRoundIndicator
  )

  val expansionLegendMark = style(
    roundLegendMark,
    expansionRoundIndicator
  )

  val evalLegendMark = style(
    roundLegendMark,
    evalRoundIndicator
  )

  // back to table (question cells etc)

  val questionCellPadding = style(
    addClassNames("pl-1", "py-2"),
  )

  val questionCell = style(
    questionCellPadding,
    width(25 rem)
  )
  val questionText = style()

  val validityCell = style(
    addClassNames("px-2"),
    width(2 rem)
  )
  val validityText = style()
  val validValidityText = style(
    validityText,
    color(validTextColor)
  )
  val invalidValidityText = style(
    validityText,
    color(invalidTextColor)
  )

  val answerCell = style()
  val answerText = style()

  val qaPairDisplay = style()

  val questionFullDescriptionCell = style(
    padding(0 px),
    margin(0 px)
  )

  val questionSourceText = style(
    questionCellPadding
  )

  val answerSourceIdCell = style(
    width(15 rem)
  )

  val dummyRow = style(
    margin(0 px),
    padding(0 px)
  )

  val inflectionDropdown = style()

  val headerDropdown = style(
    addClassNames("mx-1")
  )

  val framesetContainer = style(
    addClassNames("p-3"),
    width(30 rem)
  )
  val disabledCriterionText = style(
    color(grey(192))
  )
  val frameHeading = verbHeading
  val frameHeadingText = verbHeadingText
  val clauseSetDisplay = style(
    addClassNames("p-1")
  )
  val mutedControl = style(
    color(grey(200)),
    &.hover(
      fontWeight.bold,
      color(black)
    )
  )
  val addClauseOption = style()
  val addFrameOption = mutedControl
  val frameDeleteText = mutedControl
  val frameLinkAllValidText = mutedControl
  val clauseDeleteText = mutedControl
  val clauseDisplay = style()
  val candidateClauseChoice = style(
    backgroundColor(rgba(256, 128, 0, 0.5))
  )
  val coveredClause = style(
    backgroundColor(rgba(0, 128, 256, 0.25))
  )
  val hoveredClause = style(
    backgroundColor(rgba(64, 192, 0, 0.4))
  )

  val questionDistTable = style(
    addClassNames("m-2", "small"),
  )

  val mlmItemsBlock = style(
    wordWrap.breakWord
  )

  val mlmItemText = style(
    addClassNames("mr-1", "small"),
  )

  val questionProbCell = style(
    backgroundColor(alternatingRowBackgroundColor2)
  )

  val coveredQuestion = coveredClause
  val hoveredQuestion = hoveredClause

  val frameSpecDisplay = style(
    height(80 %%)
  )

  val frameContainer = style(
    userSelect.none
  )
  val chosenFrameContainer = style(
    borderStyle.solid,
    borderColor(c"#000"),
    borderWidth(1 px)
  )
  val chosenFrameHeading = style(
    fontStyle.italic
  )

  val frameAuditingDisplay = style(
    height(30 vh),
    auditingPaneBorder
  )
  val singleFrameAuditingDisplay = style()
  val prevFrameInstanceText = mutedControl
  val nextFrameInstanceText = mutedControl
  val substitutedArgString = style(
    fontWeight.bold
  )
  val argPlaceholder = style()
  val argSigil = style(
    fontWeight.bold
  )

  val adverbialRoles = style(
    addClassNames("pl-3")
  )
  val adverbialRole = style(
    addClassNames("px-2")
  )
  val adverbialRoleAdverb = style()

  val sigilProportionalColor = styleF.int(0 to 20)(i =>
    styleS(color(grey((20 - i) * 10)))
  )

  val sentenceLink = style()

  val goDisplay = style(
    addClassNames("mx-2")
  )
  val goLabelText = style()
  val goTextField = style()

  val matchingFrame = style(
    backgroundColor(labeledHighlightColor)
  )
  val matchingClause = style(
    backgroundColor(labeledHighlightColor)
  )

  val verbInflectionsDisplay = style()

  val frameSubheading = style(
    color(grey(128))
  )

  val paraphrasingFilterDisplay = style(
    addClassNames("pb-2")
  )

  val clusterSplittingSpecDisplay = style(
    addClassNames("pb-2")
  )

  val indexUnificationThresholdDisplay = style(
    addClassNames("pb-2")
  )

  // TODO
  val goldClauseMarkerDisplay = style()
  val goldClauseControl = style(
    position.relative,
    // display.block,
    cursor.pointer,
    unsafeChild("input")(
      position.absolute,
      zIndex(-1),
      opacity(0.0)
    ),
    marginBottom(0 px)
  )
  val goldClauseControlDisplay = style(
    position.absolute,
    top(0 px),
    left(0 px),
    width(20 px),
    height(20 px),
    backgroundColor(rgb(230, 230, 230)),
    borderRadius(2 px),
    // textAlign.center,
    &.hover(
      backgroundColor(rgb(204, 204, 204))
    ),
  )

  val goldClauseCheckmarkContainer = style(
    &.after(
      position.absolute,
      content :=! "\"\"",
      top(3 px),
      left(7 px),
      width(6 px),
      height(11 px),
      transform := "rotate(45deg)",
      borderStyle.solid,
      borderColor(c"#fff"),
      borderWidth(0 px, 2 px, 2 px, 0 px)
    )
  )
  val goldClauseXContainer = style(
    &.after(
      position.absolute,
      color(white),
      content :=! "\"\\00d7\"",
      top(0 px),
      left(0 px),
      lineHeight(18 px),
      width(20 px),
      fontSize(20 px),
      textAlign.center
    )
  )

  val goldClauseCheckLabel = style(
    goldClauseControl
  )
  val goldClauseCheck = style(
    goldClauseControlDisplay,
    goldClauseCheckmarkContainer
  )
  val goldClauseCheckCorrect = style(
    backgroundColor(c"#66FF66"),
    &.hover(
      backgroundColor(c"#55EE55")
    ),
    &.after(
      display.block
    )
  )
  val goldClauseXLabel = style(
    goldClauseControl,
    marginLeft(25 px)
  )
  val goldClauseX = style(
    goldClauseControlDisplay,
    goldClauseXContainer
  )
  val goldClauseXIncorrect = style(
    backgroundColor(c"#FF6666"),
    &.hover(
      backgroundColor(c"#EE5555")
    ),
    &.after(
      display.block
    )
  )
  val shiftedClauseTemplateDisplay = style(
    marginLeft(50 px)
  )
  val extendedXLabel = style(
    goldClauseControl
  )
  val extendedX = style(
    goldClauseControlDisplay,
    goldClauseXContainer,
    goldClauseXIncorrect,
    width(45 px),
    &.after(
      width(45 px)
    )
  )

  val clauseDecodingResultsDisplay = style()
  val clauseDecodingResultsText = style(
    fontSize(10 px),
    color(grey(128))
  )

  val paraphraseDecodingResultsText = style(
    addClassNames("ml-2"),
    clauseDecodingResultsText
  )

  val goldQAsIndicatorDisplay = style(
    addClassNames("mt-2")
  )
  val goldQAsIndicatorText = style(
    fontWeight.bold
  )
  val predQAsIndicatorDisplay = style(
    goldQAsIndicatorDisplay
  )
  val predQAsIndicatorText = style(
    goldQAsIndicatorText
  )

  val predictionRoundIndicator = style(
    roundIndicator,
    backgroundColor(predictionRoundIndicatorColor)
  )

  val goldClausesDisplay = style()
  val goldClausesHeading = style(
    goldQAsIndicatorDisplay
  )
  val goldClausesHeadingText = style(
    goldQAsIndicatorText
  )
  val goldClauseDisplay = style()
  val goldClauseText = style()

  val genericGoldMatchingArgMarker = style(
    fontStyle.italic
  )
  val goldMatchingArgMarker = style(
    fontWeight.bold
  )
  val predMatchingArgMarker = style(
    fontStyle.italic
  )
  val highlightedArgPlaceholder = style(
    backgroundColor(rgba(256, 128, 0, 0.5))
  )

  val hoveredArgMarker = hoveredQuestion
  val hoveredArgSubstitution = hoveredQuestion
  val argStructureChoiceIsChosen = style(
    backgroundColor(c"#AAEEFF")
  )
  val argStructureChoiceIsCorrectParaphrase = style(
    backgroundColor(c"#AAFFAA")
  )
  val argStructureChoiceIsIncorrectParaphrase = style(
    backgroundColor(c"#FFAAAA")
  )

  val predictedParaphraseRow = style(
    addClassNames("p-1"),
    width(100 %%)
  )
  val predictedParaphraseCell = style(
    addClassNames("py-1")
  )
  val predictedParaphraseText = style()

  val incorrectClausePredictedParaphraseRow = style(
    backgroundColor(grey(240))
  )

  val metricsTable = style(
    addClassNames("p-2"),
    fontSize(16 px)
  )

  val rowBottomBorder = style(
    borderBottom.solid,
    borderWidth(1 px)
  )

  val tableCell = style(
    addClassNames("p-2")
  )

  val numDataCell = style(
    tableCell,
    addClassNames("pl-3")
  )

  val cellProp = styleF.int(0 to 20)(i =>
    styleS(
      backgroundColor(rgba(255, 0, 0, 1.0 - (i.toDouble / 20)))
    )
  )

  val errColor = styleF.int(0 to 20)(i =>
    styleS(
      backgroundColor(rgba(255, 0, 0, i.toDouble / 20))
    )
  )

  val cellRightBorder = style(
    borderRight.solid,
    borderWidth(1 px)
  )

  val tableLeftHeader = style(
    tableCell,
    addClassNames("pr-3")
  )
}
