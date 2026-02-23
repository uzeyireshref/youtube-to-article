import streamlit as st
from google import genai
import re
from youtube_transcript_api import YouTubeTranscriptApi

# --- UI EXTRAS ---
from streamlit_extras.colored_header import colored_header
from streamlit_extras.add_vertical_space import add_vertical_space

# =========================
# CONFIG
# =========================
st.set_page_config(page_title="Podcast-to-X: Ultimate Studio", layout="wide", page_icon="⚡")

# ✅ API KEY (best practice: use st.secrets or env; keeping your style too)
API_KEY = st.secrets["GEMINI_API_KEY"]
client = genai.Client(api_key=API_KEY)

# =========================
# THEME / CSS (Minimalist & Clean)
# =========================
st.markdown(
    """
<style>
/* Ana arka plan ve font ayarı */
.stApp {
    background-color: #FFFFFF;
    color: #1A1A1A;
}

/* Metin kutularını belirginleştir */
.stTextArea textarea {
    border: 1px solid #E0E0E0 !important;
    border-radius: 8px !important;
    background-color: #F9F9F9 !important;
    color: #1A1A1A !important;
}

/* Placeholder (silik yazı) rengi */
.stTextArea textarea::placeholder {
    color: #888888 !important;
}

/* Başlıkları sadeleştir */
h1, h2, h3 {
    color: #333333;
    font-weight: 600;
}

/* Butonları modern ve sade yap */
.stButton>button {
    border-radius: 8px;
    border: 1px solid #D1D5DB;
    background-color: white;
    transition: all 0.2s;
}

.stButton>button:hover {
    border-color: #4F46E5;
    color: #4F46E5;
}

/* Ana işlem butonu (Execute Task) */
button[kind="primary"] {
    background-color: #4F46E5 !important;
    color: white !important;
    border: none !important;
}

/* Kart yapılarını basitleştir */
div[data-testid="stVerticalBlockBorderWrapper"] {
    border: 1px solid #EDEDED !important;
    border-radius: 12px !important;
    padding: 20px !important;
    background-color: white !important;
}

/* Bilgi çipleri */
.chip {
    display: inline-block;
    background: #F3F4F6;
    border: 1px solid #E5E7EB;
    padding: 4px 12px;
    border-radius: 20px;
    font-size: 12px;
    margin-right: 5px;
    color: #374151;
}
</style>
""",
    unsafe_allow_html=True,
)

# =========================
# HELPERS
# =========================
def get_video_id(url: str):
    """Extract YouTube ID from various URL formats."""
    if not url:
        return None
    pattern = r"(?:v=|\/)([0-9A-Za-z_-]{11}).*"
    match = re.search(pattern, url)
    return match.group(1) if match else None


def fetch_transcript(video_id: str):
    """Reliable transcript fetch method (your approved approach)."""
    try:
        fetched = YouTubeTranscriptApi().fetch(video_id, languages=["az", "tr", "en"])
        data = fetched.to_raw_data()
        return " ".join(item.get("text", "") for item in data).strip()
    except Exception as e:
        return f"Error: Could not fetch transcript. ({e})"


def quick_language_hint(text: str) -> str:
    """Very lightweight hint (not perfect)."""
    if not text.strip():
        return "-"
    # crude heuristics
    tr_chars = set("çğıöşüİ")
    az_chars = set("əğışçöüƏ")
    if any(c in text for c in az_chars):
        return "AZ (hint)"
    if any(c in text for c in tr_chars):
        return "TR (hint)"
    # if mostly latin and common English words
    if re.search(r"\b(the|and|you|with|this|that|is|are)\b", text.lower()):
        return "EN (hint)"
    return "Mixed/Unknown"


def clamp_text(text: str, limit: int = 30000) -> str:
    return text[:limit] if text else ""


def copy_button_html(text_to_copy: str, label: str = "📋 Copy Result"):
    """HTML button that copies to clipboard via browser JS."""
    safe = (text_to_copy or "").replace("\\", "\\\\").replace("`", "\\`").replace("$", "\\$")
    # Use JS with textarea for broad compatibility
    html = f"""
    <div style="margin-top:8px;">
      <button
        style="
          width:100%;
          padding:10px 12px;
          border-radius:12px;
          border:1px solid #1F2937;
          background:#111827;
          color:#E5E7EB;
          cursor:pointer;
        "
        onclick="
          const t = `{safe}`;
          navigator.clipboard.writeText(t).then(() => {{
            const el = document.getElementById('copy-status');
            el.innerText = '✅ Copied!';
            setTimeout(() => el.innerText = '', 1200);
          }}).catch(() => {{
            const el = document.getElementById('copy-status');
            el.innerText = '⚠️ Copy failed (browser permission).';
            setTimeout(() => el.innerText = '', 1800);
          }});
        "
      >
        {label}
      </button>
      <div id="copy-status" style="margin-top:6px;color:#9CA3AF;font-size:12px;"></div>
    </div>
    """
    st.components.v1.html(html, height=70)


# =========================
# SESSION STATE
# =========================
if "stored_text" not in st.session_state:
    st.session_state.stored_text = ""
if "final_output" not in st.session_state:
    st.session_state.final_output = ""
if "last_error" not in st.session_state:
    st.session_state.last_error = ""


# =========================
# HEADER
# =========================
colored_header(
    label="Podcast-to-X: Ultimate Studio",
    description="Transform YouTube videos into Blogs, Tweets, or Articles with AI power.",
    color_name="violet-70",
)

st.markdown(
    """
<div style="display:flex;justify-content:space-between;align-items:center;margin-top:-6px;margin-bottom:10px;">
  <div class="muted">Studio UI • Transcript → Prompt → Generate</div>
  <div class="badge">Gemini 2.5 Flash • Ready</div>
</div>
""",
    unsafe_allow_html=True,
)

add_vertical_space(1)

# =========================
# LAYOUT
# =========================
col_left, col_right = st.columns([1, 1], gap="medium")

# =========================
# LEFT: SOURCE
# =========================
with col_left:
    st.subheader("📥 1. Source Material")

    with st.container(border=True):
        st.markdown("**YouTube Transcript**")

        url_input = st.text_input(
            "YouTube URL",
            placeholder="Paste link here...",
            label_visibility="collapsed",
        )

        c1, c2 = st.columns([1, 1])
        with c1:
            if st.button("Fetch Transcript", use_container_width=True):
                v_id = get_video_id(url_input)
                if v_id:
                    with st.spinner("Downloading transcript..."):
                        st.session_state.stored_text = fetch_transcript(v_id)
                else:
                    st.error("Invalid URL: could not extract video ID.")
        with c2:
            if st.button("Clear Memory", use_container_width=True):
                st.session_state.stored_text = ""
                st.session_state.final_output = ""
                st.session_state.last_error = ""
                st.rerun()

        # Transcript stats
        t = st.session_state.stored_text or ""
        char_count = len(t)
        word_count = len(re.findall(r"\S+", t))
        lang_hint = quick_language_hint(t)

        st.markdown(
            f"""
<span class="chip">Chars: <b>{char_count:,}</b></span>
<span class="chip">Words: <b>{word_count:,}</b></span>
<span class="chip">Language: <b>{lang_hint}</b></span>
<span class="chip">Sent to model: <b>{min(char_count, 30000):,}</b> / 30,000</span>
""",
            unsafe_allow_html=True,
        )

        st.caption("Raw Transcript (Editable):")
        st.session_state.stored_text = st.text_area(
            "Transcript",
            value=st.session_state.stored_text,
            height=520,
            label_visibility="collapsed",
        )

# =========================
# RIGHT: COMMAND CENTER
# =========================
with col_right:
    st.subheader("🚀 2. Command Center")

    with st.container(border=True):
        st.markdown("**⚙️ Configuration**")

        style_choice = st.radio(
            "Select Output Style:",
            ["Strict (Pure Translation)", "Balanced (Standard)", "Creative (Social Media)"],
            index=1,
        )

        temp_map = {
            "Strict (Pure Translation)": 0.0,
            "Balanced (Standard)": 0.5,
            "Creative (Social Media)": 0.8,
        }
        selected_temp = temp_map[style_choice]

        st.markdown(
            f"<span class='chip'>Temperature: <b>{selected_temp}</b></span>"
            f"<span class='chip'>Model: <b>gemini-2.5-flash</b></span>",
            unsafe_allow_html=True,
        )

    add_vertical_space(1)

    with st.container(border=True):
        st.markdown("**✍️ Your Instruction**")
        user_task = st.text_area(
            "Prompt",
            placeholder="E.g., 'Summarize the main arguments in Turkish' or 'Create a viral Twitter thread'...",
            height=140,
            label_visibility="collapsed",
        )

        # Primary action
        if st.button("✨ EXECUTE TASK", type="primary", use_container_width=True):
            st.session_state.last_error = ""
            if st.session_state.stored_text and user_task:
                with st.spinner("Gemini is crafting your content..."):
                    full_prompt = (
                        f"TASK: {user_task}\n"
                        f"STYLE: {style_choice} (Temp: {selected_temp})\n"
                        "SYSTEM: Ignore transcript typos. Focus on content clarity.\n"
                        "SYSTEM: If the transcript is messy, infer the most likely intended meaning.\n\n"
                        f"SOURCE TEXT:\n{clamp_text(st.session_state.stored_text, 30000)}"
                    )

                    try:
                        response = client.models.generate_content(
                            model="gemini-2.5-flash",
                            contents=full_prompt,
                            config={"temperature": selected_temp},
                        )
                        st.session_state.final_output = response.text or ""
                    except Exception as e:
                        st.session_state.last_error = str(e)
                        st.session_state.final_output = ""
            else:
                st.warning("Please provide source text and instructions first.")

        if st.session_state.last_error:
            st.error(f"API Error: {st.session_state.last_error}")

    # =========================
    # RESULT
    # =========================
    if st.session_state.final_output:
        add_vertical_space(1)
        st.divider()
        st.subheader("✨ Result")

        with st.container(border=True):
            # Show output
            st.markdown(st.session_state.final_output)

            # Copy + downloads
            copy_button_html(st.session_state.final_output, label="📋 Copy Result")

            d1, d2 = st.columns([1, 1])
            with d1:
                st.download_button(
                    label="📄 Download (.txt)",
                    data=st.session_state.final_output,
                    file_name="gemini_output.txt",
                    mime="text/plain",
                    use_container_width=True,
                )
            with d2:
                st.download_button(
                    label="📝 Download (.md)",
                    data=st.session_state.final_output,
                    file_name="gemini_output.md",
                    mime="text/markdown",
                    use_container_width=True,
                )

# =========================
# FOOTER
# =========================
add_vertical_space(3)
st.caption("Powered by Google Gemini 2.5 & Streamlit • Ultimate Studio UI")

