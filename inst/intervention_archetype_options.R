# Draft options for a real-world "intervention archetype" library, analogous
# to the pathogen archetypes ("flu" / "sars_cov_2" / "measles") in
# get_parameters(). Not wired into the package yet -- this is a sketch of
# what make_intervention() calls would look like for each archetype, with
# citations and open questions noted inline. See conversation history for
# the literature review behind these numbers.
#
# Every archetype reduces to one of three shapes that make_intervention()
# already supports:
#   1. constant adder      -- delta_function ignores baseline ACH
#   2. baseline-scaled      -- delta_depends_on_baseline_ach = TRUE,
#                              delta = (multiplier - 1) * baseline_ach
#   3. target-floor adder   -- delta_depends_on_baseline_ach = TRUE,
#                              delta = max(target_ach - baseline_ach, 0)

devtools::load_all()

# =============================================================================
# 1. Open windows / doors (passive ventilation)
# =============================================================================

# Shape 3 (target-floor). Wallace et al. 2002's "3-5x baseline" multiplier
# and the hospital-room data point (0.3-0.4 ACH baseline -> ~2.2 ACH with two
# windows + a door open) don't agree on a single multiplier, and our
# household default ACH (0.5) is already in the same range as the hospital
# baseline -- so a target-floor formulation avoids double-counting a
# multiplier on top of an already-low baseline.
#
# OPEN QUESTION: target-floor (this) vs multiplier-based (3-5x baseline)?
# Whichever we pick should probably vary by setting, since "baseline ACH
# when windows are closed" differs a lot between household (0.5) and
# workplace/school (~4-5).
open_windows <- make_intervention(
  name = "open_windows",
  delta_depends_on_baseline_ach = TRUE,
  delta_function = function(ach) max(2.2 - ach, 0),
  coverage = NULL # set via set_intervention_ach()
)

# =============================================================================
# 2. Natural stack ventilation
# =============================================================================

# SKIPPED for v1. No good eACH literature found -- would need to fabricate a
# number. Revisit if a citable estimate turns up.

# =============================================================================
# 3. HVAC system upgrade (increased ventilation rate + filtration)
# =============================================================================

# Shape 3 (target-floor). "Standard" 2-4 ACH HVAC is describing an
# un-upgraded baseline, not something this archetype needs to encode --
# only the *upgraded* target (6-15 ACH range) matters here.
#
# OPEN QUESTION: is 10 ACH (midpoint of 6-15) the right target, or should
# this expose the target as a parameter so users can pick where in that
# range they want to land?
hvac_upgrade <- make_intervention(
  name = "hvac_upgrade",
  delta_depends_on_baseline_ach = TRUE,
  delta_function = function(ach, target = 10) max(target - ach, 0),
  delta_params = list(target = 10),
  coverage = NULL
)

# =============================================================================
# 4. Portable HEPA air filter
# =============================================================================

# Shape 1 (constant). Lu et al. 2023: portable HEPA cleaner gave the
# equivalent of 10 ACH in a 15x15x8 ft (~51 m3) room.
#
# OPEN QUESTION: that 51 m3 test room doesn't match any of our setting
# volumes (volume_per_person_workplace = 27, school = 10, leisure = 8,
# household = 50 -- all m3/person, not m3/room, so not directly comparable
# either). Do we (a) keep the flat +10 eACH regardless of setting, accepting
# it was calibrated in a specific room size, or (b) try to rescale by
# room/setting volume? Leaning (a) for simplicity unless someone objects --
# CADR-rated units are typically sized to a target room size by the
# manufacturer anyway, so a flat number may be the more realistic
# real-world deployment pattern.
hepa_portable <- make_intervention(
  name = "hepa_portable",
  delta_function = function() 10,
  coverage = NULL
)
# Cost context (not modelled): $200-750 purchase (residential), $120-1000
# (commercial), ~$80-120/yr filter replacement. (Lu et al. 2023)

# =============================================================================
# 5. MERV-13 filter upgrade (HVAC recirculation filtration)
# =============================================================================

# Shape 1 (constant, but see caveat below). Formula and efficiency number
# from Azimi & Stephens 2013 (Building and Environment), who derive:
#
#   delta_eACH = lambda_recirculated * eta_filter
#
# where lambda_recirculated = recirculated airflow rate / room volume (1/hr),
# and eta_filter = particle-size-weighted filtration efficiency for the
# pathogen size range of interest.
#
# eta_filter for MERV-13 = 86% (mean across 7 influenza-aerosol studies in
# Azimi & Stephens Table 4; range 81.6-89.2% depending on assumed particle
# size distribution -- already a much better-grounded number than generic
# "80-90% of particulate matter" filtration claims, since it's specifically
# weighted for infectious-aerosol-relevant particle sizes).
#
# lambda_recirculated = 1.5/hr is NOT a universal constant -- it's specific
# to Azimi & Stephens' worked office example (ASHRAE 62.1 minimum outdoor
# air at 25% of total supply airflow, remainder recirculated, divided by
# room volume). Exposed as a parameter here rather than hard-coded so a
# user with a real building's recirculation rate can supply it.
#
# OPEN QUESTION: 1.5/hr is an office-specific default. Should we have a
# different default per setting (household HVAC recirculation likely looks
# different from workplace), or just document that the default is a
# starting point and expect users to override it?
merv13_upgrade <- make_intervention(
  name = "merv13_upgrade",
  delta_function = function(recirculation_rate = 1.5, efficiency = 0.86) {
    recirculation_rate * efficiency
  },
  delta_params = list(recirculation_rate = 1.5, efficiency = 0.86),
  coverage = NULL
)
# Cost context (not modelled): ~$0.01-0.02/sq ft/yr to upgrade filters.

# =============================================================================
# 6. Upper-room UVGI (254 nm, shielded)
# =============================================================================

# Shape 1 (constant, with variation). This and far_uvc (below) were
# originally going to be a single "UVGI" archetype with a 10-184 eACH
# uncertainty range, but that range turned out to span two genuinely
# different technologies, not one technology with wide uncertainty:
#   - upper-room UVGI (254nm) must stay confined above occupants' heads
#     (harmful to skin/eyes at exposure), so only treats the upper portion
#     of room air
#   - far-UVC (222nm, below) is safe for direct occupant exposure, so can
#     treat the whole room volume -- structurally why it reaches much
#     higher eACH
#
# Point estimate 13 eACH = midpoint of the commonly-cited 10-16 eACH range
# for a properly designed installation; variation spans the wider 10-39
# eACH band to reflect fixture count/power/room geometry variation.
#
# OPEN QUESTION: deployability constraint not yet modelled -- upper-room
# UVGI needs >=2.5m ceilings and occupant-avoidance shielding. Does that
# need to be encoded as a setting restriction (e.g. error if applied to a
# setting with low assumed ceiling height), or just a documentation note?
uvgi_upper_room <- make_intervention(
  name = "uvgi_upper_room",
  delta_function = function() 13,
  variation = TRUE,
  variation_function = runif,
  variation_params = list(min = 10, max = 39),
  coverage = NULL
)
# Cost context (not modelled): typical 500 sq ft room needs 2-3 fixtures,
# $1,500-2,500 total. (CDC)

# =============================================================================
# 7. Far-UVC (222 nm, unshielded, whole-room)
# =============================================================================

# Shape 1 (constant, with variation). Conservative point estimate of 35
# eACH (the more consistently-cited lower figure across vendor/literature
# sources); upper bound reflects higher-power configurations reported in
# recent literature (one 2025 preprint reports >62 eACH at 25% of max safe
# human exposure limit).
#
# OPEN QUESTION: "Graffe et al. 2023" from the original research notes
# couldn't be located by that name/spelling during the research pass --
# the 35-184 eACH range likely came from a mix of far-UVC sources rather
# than one paper. Worth tracking down the original source material if it's
# available, to replace this placeholder citation with the real one.
far_uvc <- make_intervention(
  name = "far_uvc",
  delta_function = function() 35,
  variation = TRUE,
  variation_function = runif,
  variation_params = list(min = 35, max = 62),
  coverage = NULL
)

# =============================================================================
# 8. In-duct UV
# =============================================================================

# Shape 1, via the existing uv_to_delta() helper rather than a flat number --
# in-duct UV's effect is naturally a fraction-of-air-treated x
# inactivation-rate calculation, which is exactly what uv_to_delta(f, E_avg, k)
# already computes.
#
# OPEN QUESTION: no f / E_avg / k point estimates sourced yet for typical
# in-duct fixtures -- the original research notes only gave cost
# ($250-1,000/lamp). Needs a literature pass similar to MERV-13's before
# this archetype can ship with real numbers; placeholder values below are
# illustrative only and should NOT be used as defaults.
in_duct_uv <- make_intervention(
  name = "in_duct_uv",
  delta_function = function(f, E_avg, k) uv_to_delta(f = f, E_avg = E_avg, k = k),
  delta_params = list(f = NA, E_avg = NA, k = NA), # placeholder, not yet sourced
  coverage = NULL
)
# Cost context (not modelled): $250-1,000 per lamp/system (residential /
# light commercial).

# =============================================================================
# Open questions needing a decision before any of this gets wired into R/
# =============================================================================

# 1. open_windows: target-floor (current sketch) vs multiplier-based (3-5x
#    baseline)? Should the target/multiplier vary by setting?
# 2. hvac_upgrade: fixed target = 10 ACH, or expose as a user-facing param?
# 3. hepa_portable: flat +10 eACH regardless of setting (current sketch),
#    or rescale by room/setting volume?
# 4. merv13_upgrade: single default recirculation_rate = 1.5/hr (office-
#    derived) for all settings, or per-setting defaults?
# 5. uvgi_upper_room: does the >=2.5m ceiling / shielding constraint need
#    to be enforced in code, or just documented?
# 6. far_uvc: track down the real source behind the original "Graffe et
#    al. 2023" 35-184 eACH citation to replace the placeholder reasoning
#    above.
# 7. in_duct_uv: needs a literature pass for f / E_avg / k point estimates
#    -- not usable yet.
# 8. Should every archetype carry a `cost` field (purchase + replacement)
#    for later cost-effectiveness work, even though make_intervention()
#    doesn't support one today? Cost context is left as comments above for
#    now rather than modelled.
