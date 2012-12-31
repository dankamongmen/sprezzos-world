/*
 * Copyright (C) 2004-2012 Andrew Mihal
 *
 * This file is part of Enblend.
 *
 * Enblend is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Enblend is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Enblend; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef _MSC_VER
#define isnan _isnan
#endif // _MSC_VER

#ifdef _WIN32
// Make sure we bring in windows.h the right way
#define _STLP_VERBOSE_AUTO_LINK
#define _USE_MATH_DEFINES
#define NOMINMAX
#define VC_EXTRALEAN
#include <windows.h>
#undef DIFFERENCE
#endif  // _WIN32

#ifdef __GW32C__
#undef malloc
#define BOOST_NO_STDC_NAMESPACE 1
#endif

#include <algorithm>
#include <iostream>
#include <list>
#include <set>
#include <vector>

#include <getopt.h>
extern "C" char *optarg;
extern "C" int optind;

#ifndef _MSC_VER
#include <fenv.h>
#endif

#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <tiffconf.h>

#ifdef _WIN32
#include <io.h>
#endif

#include <boost/logic/tribool.hpp>
#include <boost/random/mersenne_twister.hpp>
#include <lcms2.h>

#include "global.h"
#include "layer_selection.h"
#include "signature.h"
#include "selector.h"
#include "self_test.h"
#include "tiff_message.h"

// Globals
const std::string command("enfuse");

// Random number generator for dithering
boost::mt19937 Twister;

// Global values from command line parameters.
std::string OutputFileName(DEFAULT_OUTPUT_FILENAME);
int Verbose = 1;                //< src::default-verbosity-level 1
int ExactLevels = 0;            // 0 means: automatically calculate maximum
bool OneAtATime = true;
boundary_t WrapAround = OpenBoundaries;
bool GimpAssociatedAlphaHack = false;
boost::tribool UseCIECAM = boost::indeterminate;
bool OutputSizeGiven = false;
int OutputWidthCmdLine = 0;
int OutputHeightCmdLine = 0;
int OutputOffsetXCmdLine = 0;
int OutputOffsetYCmdLine = 0;
std::string OutputCompression;
std::string OutputPixelType;
double WExposure = 1.0;         //< src::default-weight-exposure 1.0
AlternativePercentage ExposureLowerCutoff(0.0, true); //< src::default-exposure-lower-cutoff 0%
AlternativePercentage ExposureUpperCutoff(100.0, true); //< src::default-exposure-upper-cutoff 100%
std::string ExposureLowerCutoffGrayscaleProjector("anti-value"); //< src::default-exposure-lower-cutoff-projector anti-value
std::string ExposureUpperCutoffGrayscaleProjector("value"); //< src::default-exposure-upper-cutoff-projector value
double WContrast = 0.0;         //< src::default-weight-contrast 0.0
double WSaturation = 0.2;       //< src::default-weight-saturation 0.2
double WEntropy = 0.0;          //< src::default-weight-entropy 0.0
double WMu = 0.5;               //< src::default-exposure-mu 0.5
double WSigma = 0.2;            //< src::default-exposure-sigma 0.2
bool WSaturationIsDefault = true;
int ContrastWindowSize = 5;     //< src::default-contrast-window-size 5
std::string GrayscaleProjector;
struct EdgeFilterConfiguration {double edgeScale, lceScale, lceFactor;} FilterConfig = {
    0.0,                        //< src::default-edge-scale 0.0
    0.0,                        //< src::default-lce-scale 0.0
    0.0                         //< src::default-lce-factor 0.0
};
AlternativePercentage MinCurvature(0.0, false); //< src::default-minimum-curvature 0
int EntropyWindowSize = 3;      //< src::default-entropy-window-size 3
AlternativePercentage EntropyLowerCutoff(0.0, true); //< src::default-entropy-lower-cutoff 0%
AlternativePercentage EntropyUpperCutoff(100.0, true); //< src::default-entropy-upper-cutoff 100%
bool UseHardMask = false;
bool SaveMasks = false;
bool StopAfterMaskGeneration = false;
bool LoadMasks = false;
std::string SoftMaskTemplate("softmask-%n.tif"); //< src::default-soft-mask-template softmask-%n.tif
std::string HardMaskTemplate("hardmask-%n.tif"); //< src::default-hard-mask-template hardmask-%n.tif

TiffResolution ImageResolution;
bool OutputIsValid = true;

parameter_map Parameter;

// Globals related to catching SIGINT
#ifndef _WIN32
sigset_t SigintMask;
#endif

// Objects for ICC profiles
cmsHPROFILE InputProfile = NULL;
cmsHPROFILE XYZProfile = NULL;
cmsHPROFILE LabProfile = NULL;
cmsHTRANSFORM InputToXYZTransform = NULL;
cmsHTRANSFORM XYZToInputTransform = NULL;
cmsHTRANSFORM InputToLabTransform = NULL;
cmsHTRANSFORM LabToInputTransform = NULL;
cmsViewingConditions ViewingConditions;
cmsHANDLE CIECAMTransform = NULL;
cmsHPROFILE FallbackProfile = NULL;

Signature sig;
LayerSelectionHost LayerSelection;

#include <vigra/imageinfo.hxx>
#include <vigra/impex.hxx>
#include <vigra/sized_int.hxx>

#include <tiffio.h>

#include "common.h"
#include "enfuse.h"

#ifdef DMALLOC
#include "dmalloc.h"            // must be last #include
#endif

#ifdef _WIN32
#define strdup _strdup
#endif


// Initialize data structures for precomputed entropy and logarithm.
template <typename InputPixelType, typename ResultPixelType>
size_t enblend::Histogram<InputPixelType, ResultPixelType>::precomputedSize = 0;
template <typename InputPixelType, typename ResultPixelType>
double* enblend::Histogram<InputPixelType, ResultPixelType>::precomputedLog = NULL;
template <typename InputPixelType, typename ResultPixelType>
double* enblend::Histogram<InputPixelType, ResultPixelType>::precomputedEntropy = NULL;


#define DUMP_GLOBAL_VARIABLES(...) dump_global_variables(__FILE__, __LINE__, ##__VA_ARGS__)
void dump_global_variables(const char* file, unsigned line,
                           std::ostream& out = std::cout)
{
    out <<
        "+ " << file << ":" << line << ": state of global variables\n" <<
        "+ Verbose = " << Verbose << ", option \"--verbose\"\n" <<
        "+ OutputFileName = <" << OutputFileName << ">\n" <<
        "+ ExactLevels = " << ExactLevels << "\n" <<
        "+ OneAtATime = " << enblend::stringOfBool(OneAtATime) << ", option \"-a\"\n" <<
        "+ WrapAround = " << enblend::stringOfWraparound(WrapAround) << ", option \"--wrap\"\n" <<
        "+ GimpAssociatedAlphaHack = " << enblend::stringOfBool(GimpAssociatedAlphaHack) <<
        ", option \"-g\"\n" <<
        "+ UseCIECAM = " << UseCIECAM << ", option \"--ciecam\"\n" <<
        "+ FallbackProfile = " << (FallbackProfile ? enblend::profileDescription(FallbackProfile) : "[none]") <<
        ", option \"--fallback-profile\"\n" <<
        "+ OutputSizeGiven = " << enblend::stringOfBool(OutputSizeGiven) << ", option \"-f\"\n" <<
        "+     OutputWidthCmdLine = " << OutputWidthCmdLine << ", argument to option \"-f\"\n" <<
        "+     OutputHeightCmdLine = " << OutputHeightCmdLine << ", argument to option \"-f\"\n" <<
        "+     OutputOffsetXCmdLine = " << OutputOffsetXCmdLine << ", argument to option \"-f\"\n" <<
        "+     OutputOffsetYCmdLine = " << OutputOffsetYCmdLine << ", argument to option \"-f\"\n" <<
        "+ WExposure = " << WExposure << ", argument to option \"--exposure-weight\"\n" <<
        "+     WMu = " << WMu  << ", argument to option \"--exposure-mu\"\n" <<
        "+     WSigma = " << WSigma << ", argument to option \"--exposure-sigma\"\n" <<
        "+ ExposureLowerCutoff = {\n"
        "+     value = " << ExposureLowerCutoff.value() << ",\n" <<
        "+     is_percentage = " << enblend::stringOfBool(ExposureLowerCutoff.is_percentage()) << "\n" <<
        "+ }, first argument to option \"--exposure-cutoff\"\n" <<
        "+ ExposureUpperCutoff = {\n"
        "+     value = " << ExposureUpperCutoff.value() << ",\n" <<
        "+     is_percentage = " << enblend::stringOfBool(ExposureUpperCutoff.is_percentage()) << "\n" <<
        "+ }, second argument to option \"--exposure-cutoff\"\n" <<
        "+ ExposureLowerCutoffGrayscaleProjector = <" << ExposureLowerCutoffGrayscaleProjector <<
        ">, third argument to option \"--exposure-cutoff\"\n" <<
        "+ ExposureUpperCutoffGrayscaleProjector = <" << ExposureUpperCutoffGrayscaleProjector <<
        ">, fourth argument to option \"--exposure-cutoff\"\n" <<
        "+ WContrast = " << WContrast << ", argument to option \"--contrast-weight\"\n" <<
        "+ WSaturation = " << WSaturation << ", argument to option \"--saturation-weight\"\n" <<
        "+ WEntropy = " << WEntropy << ", argument to option \"--entropy-weight\"\n" <<
        "+ WSaturationIsDefault = " << enblend::stringOfBool(WSaturationIsDefault) << "\n" <<
        "+ ContrastWindowSize = " << ContrastWindowSize <<
        ", argument to option \"--contrast-window-size\"\n" <<
        "+ GrayscaleProjector = <" << GrayscaleProjector <<
        ">, argument to option \"--gray-projector\"\n" <<
        "+ FilterConfig = {\n" <<
        "+     edgeScale = " << FilterConfig.edgeScale << ",\n" <<
        "+     lceScale = " << FilterConfig.lceScale <<  ",\n" <<
        "+     lceFactor = " << FilterConfig.lceFactor <<  "\n" <<
        "+ }, arguments to option \"--contrast-edge-scale\"\n" <<
        "+ MinCurvature = {\n"
        "+     value = " << MinCurvature.value() << ",\n" <<
        "+     is_percentage = " << enblend::stringOfBool(MinCurvature.is_percentage()) << "\n" <<
        "+ }, arguments to option \"--contrast-min-curvature\"\n" <<
        "+ EntropyWindowSize = " << EntropyWindowSize <<
        ", argument to option \"--entropy-window-size\"\n" <<
        "+ EntropyLowerCutoff = {\n"
        "+     value = " << EntropyLowerCutoff.value() << ",\n" <<
        "+     is_percentage = " << enblend::stringOfBool(EntropyLowerCutoff.is_percentage()) << "\n" <<
        "+ }, first argument to option \"--entropy-cutoff\"\n" <<
        "+ EntropyUpperCutoff = {\n"
        "+     value = " << EntropyUpperCutoff.value() << ",\n" <<
        "+     is_percentage = " << enblend::stringOfBool(EntropyUpperCutoff.is_percentage()) << "\n" <<
        "+ }, second argument to option \"--entropy-cutoff\"\n" <<
        "+ UseHardMask = " << enblend::stringOfBool(UseHardMask) <<
        ", option \"--hard-mask\" or \"--soft-mask\"\n" <<
        "+ SaveMasks = " << enblend::stringOfBool(SaveMasks) << ", option \"--save-masks\"\n" <<
        "+     SoftMaskTemplate = <" << SoftMaskTemplate <<
        ">, first argument to option \"--save-masks\"\n" <<
        "+     HardMaskTemplate = <" << HardMaskTemplate <<
        ">, second argument to option \"--save-masks\"\n" <<
        "+ OutputCompression = <" << OutputCompression << ">, option \"--compression\"\n" <<
        "+ OutputPixelType = <" << OutputPixelType << ">, option \"--depth\"\n" <<
        "+ end of global variable dump\n";
}


/** Print information on the current version and some configuration
 * details. */
void printVersionAndExit() {
    std::cout << "enfuse " << VERSION << "\n\n";

    if (Verbose >= VERBOSE_VERSION_REPORTING) {
        std::cout <<
            "Extra feature: dmalloc support: " <<
#ifdef DMALLOC
            "yes" <<
#else
            "no" <<
#endif
            "\n";

#ifdef CACHE_IMAGES
        std::cout << "Extra feature: image cache: yes\n";
        {
#ifdef WIN32
            char lpPathBuffer[MAX_PATH];
            const DWORD dwRetVal = GetTempPath(MAX_PATH, lpPathBuffer);
            if (dwRetVal <= MAX_PATH && dwRetVal != 0) {
                std::cout << "  - cache file located in \"" << lpPathBuffer << "\"\n";
            }
#else
            const char* tmpdir = getenv("TMPDIR");
            std::cout << "  - environment variable TMPDIR ";
            if (tmpdir == NULL) {
                std::cout << "not set, cache file in default directory \"/tmp\"\n";
            } else {
                std::cout << "set, cache file located in \"" << tmpdir << "\"\n";
            }
#endif
        }
#else
        std::cout << "Extra feature: image cache: no\n";
#endif

#ifdef OPENMP
        const bool have_nested = have_openmp_nested();
        const bool have_dynamic = have_openmp_dynamic();
        std::cout <<
            "Extra feature: OpenMP: yes\n" <<
            "  - version " << OPENMP_YEAR << '-' << OPENMP_MONTH << "\n" <<
            "  - " << (have_nested ? "" : "no ") <<
            "support for nested parallelism;\n" <<
            "    nested parallelism " <<
            (have_nested && omp_get_nested() ? "enabled" : "disabled") << " by default\n" <<
            "  - " << (have_dynamic ? "" : "no ") <<
            "support for dynamic adjustment of the number of threads;\n" <<
            "    dynamic adjustment " <<
            (have_dynamic && omp_get_dynamic() ? "enabled" : "disabled") << " by default\n" <<
            "  - using " <<
            omp_get_num_procs() << " processor" << (omp_get_num_procs() >= 2 ? "s" : "") << " and up to " <<
            omp_get_max_threads() << " thread" << (omp_get_max_threads() >= 2 ? "s" : "") << "\n";
#else
        std::cout << "Extra feature: OpenMP: no\n";
#endif

        std::cout <<
            "\n" <<
            "Supported image formats: " << vigra::impexListFormats() << "\n" <<
            "Supported file extensions: " << vigra::impexListExtensions() << "\n\n";

        std::cout << "Supported following globbing algorithms:\n";
        const enblend::algorithm_list algos = enblend::known_globbing_algorithms();
        for (enblend::algorithm_list::const_iterator i = algos.begin(); i != algos.end(); ++i) {
            std::cout <<
                "  " << i->first << "\n" <<
                "    " << i->second << "\n";
        }
        std::cout << "\n";
    }

    if (Verbose >= VERBOSE_SIGNATURE_REPORTING) {
        std::cout.flush();
        std::wcout << sig.message() << L"\n\n";
        std::wcout.flush();
    }

    std::cout <<
        "Copyright (C) 2004-2012 Andrew Mihal.\n" <<
        "License GPLv2+: GNU GPL version 2 or later <http://www.gnu.org/licenses/gpl.html>\n" <<
        "This is free software: you are free to change and redistribute it.\n" <<
        "There is NO WARRANTY, to the extent permitted by law.\n" <<
        "\n" <<
        "Written by Andrew Mihal and others." <<
        endl;

    exit(0);
}


/** Print the usage information and quit. */
void printUsageAndExit(const bool error = true) {
    std::cout <<
        "Usage: enfuse [options] [--output=IMAGE] INPUT...\n" <<
        "Fuse INPUT images into a single IMAGE.\n" <<
        "\n" <<
        "INPUT... are image filenames or response filenames.  Response\n" <<
        "filenames start with an \"" << RESPONSE_FILE_PREFIX_CHAR << "\" character.\n"
        "\n" <<
        "Common options:\n" <<
        "  -V, --version          output version information and exit\n" <<
        "  -h, --help             print this help message and exit\n" <<
        "  -l, --levels=LEVELS    limit number of blending LEVELS to use (1 to " << MAX_PYRAMID_LEVELS << ");\n" <<
        "                         negative number of LEVELS decreases maximum;\n" <<
        "                         \"auto\" restores the default automatic maximization\n" <<
        "  -o, --output=FILE      write output to FILE; default: \"" << OutputFileName << "\"\n" <<
        "  -v, --verbose[=LEVEL]  verbosely report progress; repeat to\n" <<
        "                         increase verbosity or directly set to LEVEL\n" <<
        "  -w, --wrap[=MODE]      wrap around image boundary, where MODE is \"none\",\n" <<
        "                         \"horizontal\", \"vertical\", or \"both\"; default: " <<
        enblend::stringOfWraparound(WrapAround) << ";\n" <<
        "                         without argument the option selects horizontal wrapping\n" <<
        "  --compression=COMPRESSION\n" <<
        "                         set compression of output image to COMPRESSION,\n" <<
        "                         where COMPRESSION is:\n" <<
        "                         \"deflate\", \"jpeg\", \"lzw\", \"none\", \"packbits\", for TIFF files and\n" <<
        "                         0 to 100, or \"jpeg\", \"jpeg-arith\" for JPEG files,\n" <<
        "                         where \"jpeg\" and \"jpeg-arith\" accept a compression level\n" <<
        "  --layer-selector=ALGORITHM\n" <<
        "                         set the layer selector ALGORITHM;\n" <<
        "                         default: \"" << LayerSelection.name() << "\"; available algorithms are:\n";
    for (selector::algorithm_list::const_iterator i = selector::algorithms.begin();
         i != selector::algorithms.end();
         ++i) {
        std::cout << "                         \"" << (*i)->name() << "\": " << (*i)->description() << "\n";
    }
    std::cout <<
        "  --parameter=KEY1[=VALUE1][:KEY2[=VALUE2][:...]]\n" <<
        "                         set one or more KEY-VALUE pairs\n" <<
        "\n" <<
        "Extended options:\n" <<
        "  -b BLOCKSIZE           image cache BLOCKSIZE in kilobytes; default: " <<
        (vigra_ext::CachedFileImageDirector::v().getBlockSize() / 1024LL) << "KB\n" <<
        "  -c, --ciecam           use CIECAM02 to blend colors; disable with\n" <<
        "                         \"--no-ciecam\"\n" <<
        "  --fallback-profile=PROFILE-FILE\n" <<
        "                         use the ICC profile from PROFILE-FILE instead of sRGB\n" <<
        "  -d, --depth=DEPTH      set the number of bits per channel of the output\n" <<
        "                         image, where DEPTH is \"8\", \"16\", \"32\", \"r32\", or \"r64\"\n" <<
        "  -g                     associated-alpha hack for Gimp (before version 2)\n" <<
        "                         and Cinepaint\n" <<
        "  -f WIDTHxHEIGHT[+xXOFFSET+yYOFFSET]\n" <<
        "                         manually set the size and position of the output\n" <<
        "                         image; useful for cropped and shifted input\n" <<
        "                         TIFF images, such as those produced by Nona\n" <<
        "  -m CACHESIZE           set image CACHESIZE in megabytes; default: " <<
        (vigra_ext::CachedFileImageDirector::v().getAllocation() / 1048576LL) << "MB\n" <<
        "\n" <<
        "Fusion options:\n" <<
        "  --exposure-weight=WEIGHT\n" <<
        "                         weight given to well-exposed pixels\n" <<
        "                         (0 <= WEIGHT <= 1); default: " << WExposure << "\n" <<
        "  --saturation-weight=WEIGHT\n" <<
        "                         weight given to highly-saturated pixels\n" <<
        "                         (0 <= WEIGHT <= 1); default: " << WSaturation << "\n" <<
        "  --contrast-weight=WEIGHT\n" <<
        "                         weight given to pixels in high-contrast neighborhoods \n" <<
        "                         (0 <= WEIGHT <= 1); default: " << WContrast << "\n" <<
        "  --entropy-weight=WEIGHT\n" <<
        "                         weight given to pixels in high entropy neighborhoods\n" <<
        "                         (0 <= WEIGHT <= 1); default: " << WEntropy << "\n" <<
        "  --exposure-mu=MEAN     center also known as MEAN of Gaussian weighting\n" <<
        "                         function (0 <= MEAN <= 1); default: " << WMu << "\n" <<
        "  --exposure-sigma=SIGMA\n" <<
        "                         standard deviation of Gaussian weighting\n" <<
        "                         function (SIGMA > 0); default: " << WSigma << "\n" <<
        "  --soft-mask            average over all masks; this is the default\n" <<
        "  --hard-mask            force hard blend masks and no averaging on finest\n" <<
        "                         scale; this is especially useful for focus\n" <<
        "                         stacks with thin and high contrast features,\n" <<
        "                         but leads to increased noise\n" <<
        "\n" <<
        "Expert options:\n" <<
        "  --exposure-cutoff=LOWERCUTOFF[:UPPERCUTOFF[:LOWERPROJECTOR[:UPPERPROJECTOR]]]\n" <<
        "                         LOWERCUTOFF and UPPERCUTOFF are the values below\n" <<
        "                         or above of which pixels are weighted with zero\n" <<
        "                         weight in exposure weighting; append \"%\" signs\n" <<
        "                         for relative values; default: " <<
        ExposureLowerCutoff.str() << ":" << ExposureUpperCutoff.str() << ":" <<
        ExposureLowerCutoffGrayscaleProjector << ":" << ExposureUpperCutoffGrayscaleProjector << "\n" <<
        "  --contrast-window-size=SIZE\n" <<
        "                         set window SIZE for local-contrast analysis\n" <<
        "                         (SIZE >= 3); default: " << ContrastWindowSize  << "\n" <<
        "  --gray-projector=PROJECTOR\n" <<
        "                         apply gray-scale PROJECTOR in exposure or contrast\n" <<
        "                         weighing, where PROJECTOR is one of \"anti-value\",\n" <<
        "                         \"average\", \"l-star\", \"lightness\", \"luminance\",\n" <<
        "                         \"pl-star\", \"value\", or\n" <<
        "                         \"channel-mixer:RED-WEIGHT:GREEN-WEIGHT:BLUE-WEIGHT\";\n" <<
        "                         default: \"" <<
        enblend::MultiGrayscaleAccessor<vigra::UInt8, vigra::NumericTraits<vigra::UInt8>::Promote>::defaultGrayscaleAccessorName() << "\"\n" <<
        "  --contrast-edge-scale=EDGESCALE[:LCESCALE[:LCEFACTOR]]\n" <<
        "                         set scale on which to look for edges; positive\n" <<
        "                         LCESCALE switches on local contrast enhancement\n" <<
        "                         by LCEFACTOR (EDGESCALE, LCESCALE, LCEFACTOR >= 0);\n" <<
        "                         append \"%\" to LCESCALE for values relative to\n" <<
        "                         EDGESCALE; append \"%\" to LCEFACTOR for relative\n" <<
        "                         value; default: " <<
        FilterConfig.edgeScale << ":" << FilterConfig.lceScale << ":" << FilterConfig.lceFactor << "\n" <<
        "  --contrast-min-curvature=CURVATURE\n" <<
        "                         minimum CURVATURE for an edge to qualify; append\n" <<
        "                         \"%\" for relative values; default: " << MinCurvature.str() << "\n" <<
        "  --entropy-window-size=SIZE\n" <<
        "                         set window SIZE for local entropy analysis\n" <<
        "                         (SIZE >= 3); default: " << EntropyWindowSize  << "\n" <<
        "  --entropy-cutoff=LOWERCUTOFF[:UPPERCUTOFF]\n" <<
        "                         LOWERCUTOFF is the value below of which pixels are\n" <<
        "                         treated as black and UPPERCUTOFF is the value above\n" <<
        "                         of which pixels are treated as white in the entropy\n" <<
        "                         weighting; append \"%\" signs for relative values;\n" <<
        "                         default: " <<
        EntropyLowerCutoff.str() << ":" << EntropyUpperCutoff.str() << "\n" <<
        "  --save-masks[=SOFT-TEMPLATE[:HARD-TEMPLATE]]\n" <<
        "                         save weight masks in SOFT-TEMPLATE and HARD-TEMPLATE;\n" <<
        "                         conversion chars: \"%i\": mask index, \"%n\": mask number,\n" <<
        "                         \"%p\": full path, \"%d\": dirname, \"%b\": basename,\n" <<
        "                         \"%f\": filename, \"%e\": extension; lowercase characters\n" <<
        "                         refer to input images uppercase to the output image\n" <<
        "                         default: \"" << SoftMaskTemplate << "\":\"" << HardMaskTemplate << "\"\n" <<
        "  --load-masks[=SOFT-TEMPLATE[:HARD-TEMPLATE]]\n" <<
        "                         skip calculation of weight maps and use the ones \n" <<
        "                         in the files matching the templates instead.  These\n" <<
        "                         can be either hard or soft masks.  For template\n" <<
        "                         syntax see \"--save-masks\";\n" <<
        "                         default: \"" << SoftMaskTemplate << "\":\"" << HardMaskTemplate << "\"\n" <<
        "\n" <<
        "Enfuse accepts arguments to any option in uppercase as\n" <<
        "well as in lowercase letters.\n" <<
        "\n" <<
        "Report bugs at <" PACKAGE_BUGREPORT ">." <<
        endl;

    exit(error ? 1 : 0);
}


void cleanup_output(void)
{
#if DEBUG
    std::cout << "+ cleanup_output\n";
#endif

    if (!OutputIsValid) {
        std::cerr << command << ": info: remove invalid output image \"" << OutputFileName << "\"\n";
        errno = 0;
        if (unlink(OutputFileName.c_str()) != 0) {
            std::cerr << command <<
                ": warning: could not remove invalid output image \"" << OutputFileName << "\": " <<
                enblend::errorMessage(errno) << "\n";
        }
    }
}


/** Make sure all cached file images get destroyed, and hence the
 *  temporary files deleted, if we are killed.
 */
void sigint_handler(int sig)
{
    std::cerr << endl << command << ": interrupted" << endl;

    cleanup_output();

#if !defined(__GW32C__) && !defined(_WIN32)
    struct sigaction action;
    action.sa_handler = SIG_DFL;
    sigemptyset(&(action.sa_mask));
    sigaction(sig, &action, NULL);
#else
    signal(sig, SIG_DFL);
#endif
    raise(sig);
}


enum AllPossibleOptions {
    VersionOption, HelpOption, LevelsOption, OutputOption, VerboseOption,
    WrapAroundOption /* -w */, CompressionOption, LZWCompressionOption,
    BlockSizeOption, CIECAM02Option, NoCIECAM02Option, FallbackProfileOption,
    DepthOption, AssociatedAlphaOption /* -g */,
    SizeAndPositionOption /* -f */, CacheSizeOption,
    ExposureWeightOption, ExposureCutoffOption, SaturationWeightOption,
    ContrastWeightOption, EntropyWeightOption,
    ExposureMuOption /* --contrast-mu */, ExposureSigmaOption /* --contrast-sigma */,
    SoftMaskOption, HardMaskOption,
    ContrastWindowSizeOption, GrayProjectorOption, EdgeScaleOption,
    MinCurvatureOption, EntropyWindowSizeOption, EntropyCutoffOption,
    DebugOption, SaveMasksOption, LoadMasksOption,
    LayerSelectorOption
};

typedef std::set<enum AllPossibleOptions> OptionSetType;

bool contains(const OptionSetType& optionSet,
              enum AllPossibleOptions anOption)
{
    return optionSet.count(anOption) != 0;
}


/** Warn if options given at the command line have no effect. */
void warn_of_ineffective_options(const OptionSetType& optionSet)
{
    if (contains(optionSet, LoadMasksOption)) {
        if (contains(optionSet, ExposureWeightOption)) {
            std::cerr << command <<
                ": warning: option \"--exposure-weight\" has no effect with \"--load-masks\"" << endl;
        }
        if (contains(optionSet, ExposureCutoffOption)) {
            std::cerr << command <<
                ": warning: option \"--exposure-cutoff\" has no effect with \"--load-masks\"" << endl;
        }
        if (contains(optionSet, SaturationWeightOption)) {
            std::cerr << command <<
                ": warning: option \"--saturation-weight\" has no effect with \"--load-masks\"" << endl;
        }
        if (contains(optionSet, ContrastWeightOption)) {
            std::cerr << command <<
                ": warning: option \"--contrast-weight\" has no effect with \"--load-masks\"" << endl;
        }
        if (contains(optionSet, EntropyWeightOption)) {
            std::cerr << command <<
                ": warning: option \"--entropy-weight\" has no effect with \"--load-masks\"" << endl;
        }
        if (contains(optionSet, ExposureMuOption)) {
            std::cerr << command <<
                ": warning: option \"--exposure-mu\" has no effect with \"--load-masks\"" << endl;
        }
        if (contains(optionSet, ExposureSigmaOption)) {
            std::cerr << command <<
                ": warning: option \"--exposure-sigma\" has no effect with \"--load-masks\"" << endl;
        }
        if (contains(optionSet, ContrastWindowSizeOption)) {
            std::cerr << command <<
                ": warning: option \"--contrast-window-size\" has no effect with \"--load-masks\"" << endl;
        }
        if (contains(optionSet, GrayProjectorOption)) {
            std::cerr << command <<
                ": warning: option \"--gray-projector\" has no effect with \"--load-masks\"" << endl;
        }
        if (contains(optionSet, EdgeScaleOption)) {
            std::cerr << command <<
                ": warning: option \"--contrast-edge-scale\" has no effect with \"--load-masks\"" << endl;
        }
        if (contains(optionSet, MinCurvatureOption)) {
            std::cerr << command <<
                ": warning: option \"--contrast-min-curvature\" has no effect with \"--load-masks\"" << endl;
        }
        if (contains(optionSet, EntropyWindowSizeOption)) {
            std::cerr << command <<
                ": warning: option \"--entropy-window-size\" has no effect with \"--load-masks\"" << endl;
        }
        if (contains(optionSet, EntropyCutoffOption)) {
            std::cerr << command <<
                ": warning: option \"--entropy-cutoff\" has no effect with \"--load-masks\"" << endl;
        }
    }

    if (contains(optionSet, SaveMasksOption) && !contains(optionSet, OutputOption)) {
        if (contains(optionSet, LevelsOption)) {
            std::cerr << command <<
                ": warning: option \"--levels\" has no effect with \"--save-masks\" and no \"--output\"" <<
                endl;
        }
        if (contains(optionSet, WrapAroundOption)) {
            std::cerr << command <<
                ": warning: option \"--wrap\" has no effect with \"--save-masks\" and no \"--output\"" <<
                endl;
        }
        if (contains(optionSet, CompressionOption)) {
            std::cerr << command <<
                ": warning: option \"--compression\" has no effect with \"--save-masks\" and no \"--output\"" <<
                endl;
        }
        if (contains(optionSet, DepthOption)) {
            std::cerr << command <<
                ": warning: option \"--depth\" has no effect with \"--save-masks\" and no \"--output\"" <<
                endl;
        }
        if (contains(optionSet, SizeAndPositionOption)) {
            std::cerr << command <<
                ": warning: option \"-f\" has no effect with \"--save-masks\" and no \"--output\"" << endl;
        }
    }

    if (WExposure == 0.0 && contains(optionSet, ExposureWeightOption)) {
        if (contains(optionSet, ExposureMuOption)) {
            std::cerr << command <<
                ": warning: option \"--exposure-mu\" has no effect as exposure weight\n" <<
                command <<
                ": warning:     is zero" <<
                endl;
        }
        if (contains(optionSet, ExposureSigmaOption)) {
            std::cerr << command <<
                ": warning: option \"--exposure-sigma\" has no effect as exposure weight\n" <<
                command <<
                ": warning:     is zero" <<
                endl;
        }
    }

    if (WExposure == 0.0 && contains(optionSet, ExposureCutoffOption)) {
        std::cerr << command <<
            ": warning: option \"--exposure-cutoff\" has no effect as exposure weight\n" <<
            command <<
            ": warning:     is zero" <<
            endl;
    }

    if (WContrast == 0.0 && contains(optionSet, ContrastWindowSizeOption)) {
        std::cerr << command <<
            ": warning: option \"--contrast-window-size\" has no effect as contrast\n" <<
            command <<
            ": warning:     weight is zero" <<
            endl;
    }

    if (WExposure == 0.0 && WContrast == 0.0 && contains(optionSet, GrayProjectorOption)) {
        std::cerr << command <<
            ": warning: option \"--gray-projector\" has no effect as exposure\n" <<
            command <<
            ": warning:     and contrast weight both are zero" <<
            endl;
    }

    if (WContrast == 0.0) {
        if (contains(optionSet, EdgeScaleOption)) {
            std::cerr << command <<
                ": warning: option \"--contrast-edge-scale\" has no effect as contrast\n" <<
                command <<
                ": warning:     weight is zero" <<
                endl;
        }
        if (contains(optionSet, MinCurvatureOption)) {
            std::cerr << command <<
                ": warning: option \"--contrast-min-curvature\" has no effect as contrast\n" <<
                command <<
                ": warning:     weight is zero" <<
                endl;
        }
    } else {
        if (FilterConfig.edgeScale > 0.0 &&
            contains(optionSet, ContrastWindowSizeOption) && MinCurvature.value() <= 0.0) {
            std::cerr << command <<
                ": warning: option \"--contrast-window-size\" has no effect as\n" <<
                command <<
                ": warning:     EDGESCALE in \"--contrast-edge-scale\" is positive and" <<
                command <<
                ": warning:     \"--contrast-min-curvature\" is non-positive" <<
                endl;
        }
    }

    if (WEntropy == 0.0) {
        if (contains(optionSet, EntropyWindowSizeOption)) {
            std::cerr << command <<
                ": warning: option \"--entropy-window-size\" has no effect as\n" <<
                command <<
                ": warning:     entropy weight is zero" <<
                endl;
        }
        if (contains(optionSet, EntropyCutoffOption)) {
            std::cerr << command <<
                ": warning: option \"--entropy-cutoff\" has no effect as entropy\n" <<
                command <<
                ": warning:     weight is zero" <<
                endl;
        }
    }

    if (contains(optionSet, CompressionOption) &&
        !(enblend::getFileType(OutputFileName) == "TIFF" ||
          enblend::getFileType(OutputFileName) == "JPEG")) {
        std::cerr << command <<
            ": warning: compression is not supported with output\n" <<
            command <<
            ": warning:     file type \"" <<
            enblend::getFileType(OutputFileName) << "\"" <<
            endl;
    }

    if (contains(optionSet, AssociatedAlphaOption) &&
        enblend::getFileType(OutputFileName) != "TIFF") {
        std::cerr << command <<
            ": warning: option \"-g\" has no effect with output\n" <<
            command <<
            ": warning:     file type \"" <<
            enblend::getFileType(OutputFileName) << "\"" <<
            endl;
    }

#ifndef CACHE_IMAGES
    if (contains(optionSet, CacheSizeOption)) {
        std::cerr << command <<
            ": warning: option \"-m\" has no effect in this " << command << " binary,\n" <<
            command <<
            ": warning:     because it was compiled without image cache" <<
            endl;
    }

    if (contains(optionSet, BlockSizeOption)) {
        std::cerr << command <<
            ": warning: option \"-b\" has no effect in this " << command << " binary,\n" <<
            command <<
            ": warning:     because it was compiled without image cache" <<
            endl;
    }
#endif
}


void
fill_mask_templates(const char* an_option_argument,
                    std::string& a_soft_mask_template, std::string& a_hard_mask_template,
                    const std::string& an_option_name)
{
    if (an_option_argument != NULL && *an_option_argument != 0) {
        char* s = new char[strlen(an_option_argument) + 1];
        strcpy(s, an_option_argument);

        char* save_ptr = NULL;
        char* token = enblend::strtoken_r(s, PATH_OPTION_DELIMITERS, &save_ptr);
        a_soft_mask_template = token;
        token = enblend::strtoken_r(NULL, PATH_OPTION_DELIMITERS, &save_ptr);
        if (token != NULL && *token != 0) {
            a_hard_mask_template = token;
        }

        token = enblend::strtoken_r(NULL, PATH_OPTION_DELIMITERS, &save_ptr);
        if (token != NULL && *token != 0) {
            std::cerr << command
                      << ": warning: ignoring trailing garbage in \"" << an_option_name << "\"" << endl;
        }

        delete [] s;
    }
}


int process_options(int argc, char** argv)
{
    enum OptionId {
        OPTION_ID_OFFSET = 1023,    // Ids start at 1024
        CompressionId,
        WeightExposureId,
        WeightContrastId,
        WeightSaturationId,
        WeightMuId,
        WeightSigmaId,
        MinCurvatureId,
        EdgeScaleId,
        ContrastWindowSizeId,
        HardMaskId,
        GrayProjectorId,
        WeightEntropyId,
        EntropyWindowSizeId,
        EntropyCutoffId,
        SoftMaskId,
        VerboseId,
        HelpId,
        VersionId,
        DepthId,
        OutputId,
        SaveMasksId,
        WrapAroundId,
        LevelsId,
        CiecamId,
        NoCiecamId,
        FallbackProfileId,
        ExposureCutoffId,
        LoadMasksId,
        LayerSelectorId,
        ParameterId,
        NoParameterId
    };

    static struct option long_options[] = {
        {"compression", required_argument, 0, CompressionId},
        {"exposure-weight", required_argument, 0, WeightExposureId},
        {"contrast-weight", required_argument, 0, WeightContrastId},
        {"saturation-weight", required_argument, 0, WeightSaturationId},
        {"exposure-mu", required_argument, 0, WeightMuId},
        {"exposure-sigma", required_argument, 0, WeightSigmaId},
        {"contrast-min-curvature", required_argument, 0, MinCurvatureId},
        {"contrast-edge-scale", required_argument, 0, EdgeScaleId},
        {"contrast-window-size", required_argument, 0, ContrastWindowSizeId},
        {"hard-mask", no_argument, 0, HardMaskId},
        {"gray-projector", required_argument, 0, GrayProjectorId},
        {"entropy-weight", required_argument, 0, WeightEntropyId},
        {"entropy-window-size", required_argument, 0, EntropyWindowSizeId},
        {"entropy-cutoff", required_argument, 0, EntropyCutoffId},
        {"soft-mask", no_argument, 0, SoftMaskId},
        {"verbose", optional_argument, 0, VerboseId},
        {"help", no_argument, 0, HelpId},
        {"version", no_argument, 0, VersionId},
        {"depth", required_argument, 0, DepthId},
        {"output", required_argument, 0, OutputId},
        {"save-mask", optional_argument, 0, SaveMasksId}, // singular form: not documented, not deprecated
        {"save-masks", optional_argument, 0, SaveMasksId},
        {"wrap", optional_argument, 0, WrapAroundId},
        {"levels", required_argument, 0, LevelsId},
        {"ciecam", no_argument, 0, CiecamId},
        {"no-ciecam", no_argument, 0, NoCiecamId},
        {"fallback-profile", required_argument, 0, FallbackProfileId},
        {"exposure-cutoff", required_argument, 0, ExposureCutoffId},
        {"load-mask", optional_argument, 0, LoadMasksId}, // singular form: not documented, not deprecated
        {"load-masks", optional_argument, 0, LoadMasksId},
        {"layer-selector", required_argument, 0, LayerSelectorId},
        {"parameter", required_argument, 0, ParameterId},
        {"no-parameter", required_argument, 0, NoParameterId},
        {0, 0, 0, 0}
    };

    bool failed = false;
    bool justPrintVersion = false;
    bool justPrintUsage = false;
    OptionSetType optionSet;

    opterr = 0;       // we have our own "unrecognized option" message
    while (true) {
        int option_index;
        const int code = getopt_long(argc, argv, "Vb:cd:f:ghl:m:o:v::w::",
                                     long_options, &option_index);

        if (code == -1) {
            break;
        }

        switch (code) {
        case HardMaskId:
            UseHardMask = true;
            optionSet.insert(HardMaskOption);
            break;

        case SoftMaskId:
            UseHardMask = false;
            optionSet.insert(SoftMaskOption);
            break;

        case 'h': // FALLTHROUGH
        case HelpId:
            justPrintUsage = true;
            optionSet.insert(HelpOption);
            break;

        case 'V': // FALLTHROUGH
        case VersionId:
            justPrintVersion = true;
            optionSet.insert(VersionOption);
            break;

        case 'w': // FALLTHROUGH
        case WrapAroundId:
            if (optarg != NULL && *optarg != 0) {
                WrapAround = enblend::wraparoundOfString(optarg);
                if (WrapAround == UnknownWrapAround) {
                    std::cerr << command
                              << ": unrecognized wrap-around mode \"" << optarg << "\"\n" << endl;
                    failed = true;
                }
            } else {
                WrapAround = HorizontalStrip;
            }
            optionSet.insert(WrapAroundOption);
            break;

        case MinCurvatureId: {
            char *tail;
            errno = 0;
            MinCurvature.set_value(strtod(optarg, &tail));
            if (errno == 0) {
                if (*tail == 0) {
                    MinCurvature.set_percentage(false);
                } else if (strcmp(tail, "%") == 0) {
                    MinCurvature.set_percentage(true);
                } else {
                    std::cerr << command << ": unrecognized minimum gradient \""
                              << optarg << "\" specification." << endl;
                    failed = true;
                }
            } else {
                std::cerr << command << ": illegal numeric format \""
                          << optarg << "\" for minimum gradient: "
                          << enblend::errorMessage(errno) << endl;
                failed = true;
            }
            optionSet.insert(MinCurvatureOption);
            break;
        }

        case EdgeScaleId: {
            char* s = new char[strlen(optarg) + 1];
            strcpy(s, optarg);
            char* save_ptr = NULL;
            char* token = enblend::strtoken_r(s, NUMERIC_OPTION_DELIMITERS, &save_ptr);
            char* tail;

            if (token == NULL || *token == 0) {
                std::cerr << command << ": no scale given to \"--contrast-edge-scale\".  "
                          << "scale is required." << endl;
                failed = true;
            } else {
                errno = 0;
                FilterConfig.edgeScale = strtod(token, &tail);
                if (errno == 0) {
                    if (*tail != 0) {
                        std::cerr << command << ": could not decode \"" << tail
                                  << "\" in edge scale specification \""
                                  << token << "\" for edge scale." << endl;
                        failed = true;
                    }
                } else {
                    std::cerr << command << ": illegal numeric format \""
                              << token << "\" for edge scale: "
                              << enblend::errorMessage(errno) << endl;
                    failed = true;
                }
            }

            token = enblend::strtoken_r(NULL, NUMERIC_OPTION_DELIMITERS, &save_ptr);
            if (token != NULL && *token != 0) {
                errno = 0;
                FilterConfig.lceScale = strtod(token, &tail);
                if (errno == 0) {
                    if (strcmp(tail, "%") == 0) {
                        FilterConfig.lceScale *= FilterConfig.edgeScale / 100.0;
                    } else if (*tail != 0) {
                        std::cerr << command << ": could not decode \"" << tail
                                  << "\" in specification \"" << token
                                  << "\" for LCE-scale." << endl;
                        failed = true;
                    }
                } else {
                    std::cerr << command << ": illegal numeric format \""
                              << token << "\" for LCE-Scale: "
                              << enblend::errorMessage(errno) << endl;
                    failed = true;
                }
            }

            token = enblend::strtoken_r(NULL, NUMERIC_OPTION_DELIMITERS, &save_ptr);
            if (token != NULL && *token != 0) {
                errno = 0;
                FilterConfig.lceFactor = strtod(token, &tail);
                if (errno == 0) {
                    if (strcmp(tail, "%") == 0) {
                        FilterConfig.lceFactor /= 100.0;
                    } else if (*tail != 0) {
                        std::cerr << command << ": could not decode \"" << tail
                                  << "\" in specification \"" << token
                                  << "\" for LCE-factor." << endl;
                        failed = true;
                    }
                } else {
                    std::cerr << command << ": illegal numeric format \""
                              << token << "\" for LCE-factor: "
                              << enblend::errorMessage(errno) << endl;
                    failed = true;
                }
            }

            if (save_ptr != NULL && *save_ptr != 0) {
                std::cerr << command << ": warning: ignoring trailing garbage \""
                          << save_ptr << "\" in argument to \"--contrast-edge-scale\"" << endl;
            }

            delete [] s;
            optionSet.insert(EdgeScaleOption);
            break;
        }

        case EntropyCutoffId: {
            char* s = new char[strlen(optarg) + 1];
            strcpy(s, optarg);
            char* save_ptr = NULL;
            char* token = enblend::strtoken_r(s, NUMERIC_OPTION_DELIMITERS, &save_ptr);
            char* tail;

            if (token == NULL || *token == 0) {
                std::cerr << command << ": no scale given to \"--entropy-cutoff\".  "
                          << "lower cutoff is required." << endl;
                failed = true;
            } else {
                errno = 0;
                EntropyLowerCutoff.set_value(strtod(token, &tail));
                if (errno == 0) {
                    if (*tail == 0) {
                        EntropyLowerCutoff.set_percentage(false);
                    } else if (strcmp(tail, "%") == 0) {
                        EntropyLowerCutoff.set_percentage(true);
                    } else {
                        std::cerr << command << ": unrecognized entropy's lower cutoff \""
                                  << tail << "\" in \"" << token << "\"" << endl;
                        failed = true;
                    }
                } else {
                    std::cerr << command << ": illegal numeric format \""
                              << token << "\" of entropy's lower cutoff: "
                              << enblend::errorMessage(errno) << endl;
                    failed = true;
                }
            }

            token = enblend::strtoken_r(NULL, NUMERIC_OPTION_DELIMITERS, &save_ptr);
            if (token != NULL && *token != 0) {
                errno = 0;
                EntropyUpperCutoff.set_value(strtod(token, &tail));
                if (errno == 0) {
                    if (*tail == 0) {
                        EntropyUpperCutoff.set_percentage(false);
                    } else if (strcmp(tail, "%") == 0) {
                        EntropyUpperCutoff.set_percentage(true);
                    } else {
                        std::cerr << command << ": unrecognized entropy's upper cutoff \""
                                  << tail << "\" in \"" << token << "\"" << endl;
                        failed = true;
                    }
                } else {
                    std::cerr << command << ": illegal numeric format \""
                              << token << "\" of entropy's upper cutoff: "
                              << enblend::errorMessage(errno) << endl;
                    failed = true;
                }
            }

            if (save_ptr != NULL && *save_ptr != 0) {
                std::cerr << command << ": warning: ignoring trailing garbage \""
                          << save_ptr << "\" in argument to \"--entropy-cutoff\"" << endl;
            }

            delete [] s;
            optionSet.insert(EntropyCutoffOption);
            break;
        }

        case ExposureCutoffId: {
            char* s = new char[strlen(optarg) + 1];
            strcpy(s, optarg);
            char* save_ptr = NULL;
            char* token = enblend::strtoken_r(s, NUMERIC_OPTION_DELIMITERS, &save_ptr);
            char* tail;

            if (token == NULL || *token == 0) {
                std::cerr << command << ": no scale given to \"--exposure-cutoff\".  "
                          << "lower cutoff is required." << endl;
                failed = true;
            } else {
                errno = 0;
                ExposureLowerCutoff.set_value(strtod(token, &tail));
                if (errno == 0) {
                    if (*tail == 0) {
                        ExposureLowerCutoff.set_percentage(false);
                    } else if (strcmp(tail, "%") == 0) {
                        ExposureLowerCutoff.set_percentage(true);
                    } else {
                        std::cerr << command << ": unrecognized exposure's lower cutoff \""
                                  << tail << "\" in \"" << token << "\"" << endl;
                        failed = true;
                    }
                } else {
                    std::cerr << command << ": illegal numeric format \""
                              << token << "\" of exposure's lower cutoff: "
                              << enblend::errorMessage(errno) << endl;
                    failed = true;
                }
            }

            token = enblend::strtoken_r(NULL, NUMERIC_OPTION_DELIMITERS, &save_ptr);
            if (token != NULL && *token != 0) {
                errno = 0;
                ExposureUpperCutoff.set_value(strtod(token, &tail));
                if (errno == 0) {
                    if (*tail == 0) {
                        ExposureUpperCutoff.set_percentage(false);
                    } else if (strcmp(tail, "%") == 0) {
                        ExposureUpperCutoff.set_percentage(true);
                    } else {
                        std::cerr << command << ": unrecognized exposure's upper cutoff \""
                                  << tail << "\" in \"" << token << "\"" << endl;
                        failed = true;
                    }
                } else {
                    std::cerr << command << ": illegal numeric format \""
                              << token << "\" of exposure's upper cutoff: "
                              << enblend::errorMessage(errno) << endl;
                    failed = true;
                }
            }

            token = enblend::strtoken_r(NULL, NUMERIC_OPTION_DELIMITERS, &save_ptr);
            if (token != NULL && *token != 0) {
                ExposureLowerCutoffGrayscaleProjector = token;
            }

            token = enblend::strtoken_r(NULL, NUMERIC_OPTION_DELIMITERS, &save_ptr);
            if (token != NULL && *token != 0) {
                ExposureUpperCutoffGrayscaleProjector = token;
            }

            if (save_ptr != NULL && *save_ptr != 0) {
                std::cerr << command << ": warning: ignoring trailing garbage \""
                          << save_ptr << "\" in argument to \"--exposure-cutoff\"" << endl;
            }

            delete [] s;
            optionSet.insert(ExposureCutoffOption);
            break;
        }

        case CompressionId:
            if (optarg != NULL && *optarg != 0) {
                std::string upper_opt(optarg);
                boost::algorithm::to_upper(upper_opt);
                if (upper_opt == "NONE") {
                    ;           // stick with default
                } else if (upper_opt == "DEFLATE" || upper_opt == "LZW" || upper_opt == "PACKBITS") {
                    OutputCompression = upper_opt;
                } else if (upper_opt.find_first_not_of("0123456789") == std::string::npos) {
                    OutputCompression = "JPEG QUALITY=" + upper_opt;
                } else if (enblend::starts_with(upper_opt, "JPEG") || enblend::starts_with(upper_opt, "JPEG-ARITH")) {
                    const std::string::size_type delimiter_position = upper_opt.find_first_of(NUMERIC_OPTION_DELIMITERS);
                    if (delimiter_position == std::string::npos) {
                        if (upper_opt == "JPEG" || upper_opt == "JPEG-ARITH") {
                            OutputCompression = upper_opt;
                        } else {
                            std::cerr << command << ": trailing garbage in JPEG compression \"" << optarg << "\"" << std::endl;
                            failed = true;
                        }
                    } else {
                        const std::string algorithm(upper_opt.substr(0, delimiter_position));
                        if (algorithm == "JPEG" || algorithm == "JPEG-ARITH") {
                            const std::string level(upper_opt.substr(delimiter_position + 1U));
                            if (level.length() >= 1U && level.find_first_not_of("0123456789") == std::string::npos) {
                                upper_opt.replace(delimiter_position, 1U, " QUALITY=");
                                OutputCompression = upper_opt;
                            } else {
                                std::cerr << command << ": invalid JPEG compression level \"" << level << "\"" << std::endl;
                                failed = true;
                            }
                        } else {
                            std::cerr << command << ": unrecognized JPEG compression \"" << optarg << "\"" << std::endl;
                            failed = true;
                        }
                    }
                } else {
                    std::cerr << command << ": unrecognized compression \"" << optarg << "\"" << std::endl;
                    failed = true;
                }
            } else {
                std::cerr << command << ": option \"--compression\" requires an argument" << std::endl;
                failed = true;
            }
            optionSet.insert(CompressionOption);
            break;

        case GrayProjectorId:
            if (optarg != NULL && *optarg != 0) {
                GrayscaleProjector = optarg;
            } else {
                std::cerr << command << ": option \"--gray-projector\" requires an argument" << endl;
                failed = true;
            }
            optionSet.insert(GrayProjectorOption);
            break;

        case 'd': // FALLTHROUGH
        case DepthId:
            if (optarg != NULL && *optarg != 0) {
                OutputPixelType = enblend::outputPixelTypeOfString(optarg);
            } else {
                std::cerr << command << ": options \"-d\" or \"--depth\" require arguments" << endl;
                failed = true;
            }
            optionSet.insert(DepthOption);
            break;

        case 'o': // FALLTHROUGH
        case OutputId:
            if (contains(optionSet, OutputOption)) {
                std::cerr << command
                          << ": warning: more than one output file specified"
                          << endl;
            }
            if (optarg != NULL && *optarg != 0) {
                OutputFileName = optarg;
            } else {
                std::cerr << command << ": options \"-o\" or \"--output\" require arguments" << endl;
                failed = true;
            }
            optionSet.insert(OutputOption);
            break;

        case LoadMasksId:
            fill_mask_templates(optarg, SoftMaskTemplate, HardMaskTemplate, "--load-masks");
            LoadMasks = true;
            optionSet.insert(LoadMasksOption);
            break;

        case SaveMasksId:
            fill_mask_templates(optarg, SoftMaskTemplate, HardMaskTemplate, "--save-masks");
            SaveMasks = true;
            optionSet.insert(SaveMasksOption);
            break;

        case WeightExposureId:
            if (optarg != NULL && *optarg != 0) {
                WExposure = enblend::numberOfString(optarg,
                                                    _1 >= 0.0, //< src::minimum-weight-exposure 0
                                                    "exposure weight less than 0; will use 0",
                                                    0.0,
                                                    _1 <= 1.0, //< src::maximum-weight-exposure 1
                                                    "exposure weight greater than 1; will use 1",
                                                    1.0);
            } else {
                std::cerr << command << ": option \"--exposure-weight\" requires an argument" << endl;
                failed = true;
            }
            optionSet.insert(ExposureWeightOption);
            break;

        case WeightContrastId:
            if (optarg != NULL && *optarg != 0) {
                WContrast = enblend::numberOfString(optarg,
                                                    _1 >= 0.0, //< src::minimum-weight-contrast 0
                                                    "contrast weight less than 0; will use 0",
                                                    0.0,
                                                    _1 <= 1.0, //< src::maximum-weight-contrast 1
                                                    "contrast weight greater than 1; will use 1",
                                                    0.0);
            } else {
                std::cerr << command << ": option \"--contrast-weight\" requires an argument" << endl;
                failed = true;
            }
            optionSet.insert(ContrastWeightOption);
            break;

        case WeightSaturationId:
            if (optarg != NULL && *optarg != 0) {
                WSaturation = enblend::numberOfString(optarg,
                                                      _1 >= 0.0, //< src::minimum-weight-saturation 0
                                                      "saturation weight less than 0; will use 0",
                                                      0.0,
                                                      _1 <= 1.0, //< src::maximum-weight-saturation 1
                                                      "saturation weight greater than 1; will use 1",
                                                      1.0);
            } else {
                std::cerr << command << ": option \"--saturation-weight\" requires an argument" << endl;
                failed = true;
            }
            WSaturationIsDefault = false;
            optionSet.insert(SaturationWeightOption);
            break;

        case WeightMuId:
            if (optarg != NULL && *optarg != 0) {
                WMu = enblend::numberOfString(optarg,
                                              _1 >= 0.0, //< src::minimum-exposure-mu 0
                                              "exposure center value (mu) less than 0; will use 0",
                                              0.0,
                                              _1 <= 1.0, //< src::maximum-exposure-mu 1
                                              "exposure center value (mu) geater than 1; will use 1",
                                              1.0);
            } else {
                std::cerr << command << ": option \"--exposure-mu\" requires an argument" << endl;
                failed = true;
            }
            optionSet.insert(ExposureMuOption);
            break;

        case WeightSigmaId:
            if (optarg != NULL && *optarg != 0) {
                WSigma = enblend::numberOfString(optarg,
                                                 _1 >= 0.0, //< src::minimum-exposure-sigma 0
                                                 "exposure standard deviation (sigma) less than 0; will use 1/1024",
                                                 1.0 / 1024.0);
            } else {
                std::cerr << command << ": option \"--exposure-sigma\" requires an argument" << endl;
                failed = true;
            }
            optionSet.insert(ExposureSigmaOption);
            break;

        case WeightEntropyId:
            if (optarg != NULL && *optarg != 0) {
                WEntropy = enblend::numberOfString(optarg,
                                                   _1 >= 0.0, //< src::minimum-weight-entropy 0
                                                   "entropy weight less than 0; will use 0",
                                                   0.0,
                                                   _1 <= 1.0, //< src::maximum-weight-entropy 1
                                                   "entropy weight greater than 1; will use 1",
                                                   1.0);
            } else {
                std::cerr << command << ": option \"--entropy-weight\" requires an argument" << endl;
                failed = true;
            }
            optionSet.insert(EntropyWeightOption);
            break;

        case 'v': // FALLTHROUGH
        case VerboseId:
            if (optarg != NULL && *optarg != 0) {
                Verbose = enblend::numberOfString(optarg,
                                                  _1 >= 0, //< src::minimum-verbosity-level 0
                                                  "verbosity level less than 0; will use 0",
                                                  0);
            } else {
                Verbose++;
            }
            optionSet.insert(VerboseOption);
            break;

        case ContrastWindowSizeId:
            if (optarg != NULL && *optarg != 0) {
                ContrastWindowSize =
                    enblend::numberOfString(optarg,
                                            _1 >= 3, //< src::minimum-contrast-window-size 3
                                            "contrast window size to small; will use size = 3",
                                            3);
                if (ContrastWindowSize % 2 != 1) {
                    std::cerr << command << ": warning: contrast window size \""
                              << ContrastWindowSize << "\" is even; increasing size to next odd number"
                              << endl;
                    ContrastWindowSize++;
                }
            } else {
                std::cerr << command << ": option \"--contrast-window-size\" requires an argument" << endl;
                failed = true;
            }
            optionSet.insert(ContrastWindowSizeOption);
            break;

        case EntropyWindowSizeId:
            if (optarg != NULL && *optarg != 0) {
                EntropyWindowSize =
                    enblend::numberOfString(optarg,
                                            _1 >= 3, //< src::minimum-entropy-window-size 3
                                            "entropy window size to small; will use size = 3",
                                            3);
                if (EntropyWindowSize % 2 != 1) {
                    std::cerr << command << ": warning: entropy window size \""
                              << EntropyWindowSize << "\" is even; increasing size to next odd number"
                              << endl;
                    EntropyWindowSize++;
                }
            } else {
                std::cerr << command << ": option \"--entropy-window-size\" requires an argument" << endl;
                failed = true;
            }
            optionSet.insert(EntropyWindowSizeOption);
            break;

        case 'b':
            if (optarg != NULL && *optarg != 0) {
                const int cache_block_size =
                    enblend::numberOfString(optarg,
                                            _1 >= 1, //< src::minimum-cache-block-size 1@dmn{KB}
                                            "cache block size must be 1 KB or more; will use 1 KB",
                                            1);
                vigra_ext::CachedFileImageDirector::v().setBlockSize(static_cast<long long>(cache_block_size) << 10);
            } else {
                std::cerr << command << ": option \"-b\" requires an argument" << endl;
                failed = true;
            }
            optionSet.insert(BlockSizeOption);
            break;

        case 'c': // FALLTHROUGH
        case CiecamId:
            UseCIECAM = true;
            optionSet.insert(CIECAM02Option);
            break;

        case NoCiecamId:
            UseCIECAM = false;
            optionSet.insert(NoCIECAM02Option);
            break;

        case FallbackProfileId:
            if (enblend::can_open_file(optarg)) {
                FallbackProfile = cmsOpenProfileFromFile(optarg, "r");
                if (FallbackProfile == NULL) {
                    std::cerr << command << ": failed to open fallback ICC profile file \"" << optarg << "\"\n";
                    exit(1);
                }
            } else {
                exit(1);
            }
            optionSet.insert(FallbackProfileOption);
            break;

        case 'f':
            if (optarg != NULL && *optarg != 0) {
                const int n = sscanf(optarg,
                                     "%dx%d+%d+%d",
                                     &OutputWidthCmdLine, &OutputHeightCmdLine,
                                     &OutputOffsetXCmdLine, &OutputOffsetYCmdLine);
                if (n == 4) {
                    ; // ok: full geometry string
                } else if (n == 2) {
                    OutputOffsetXCmdLine = 0;
                    OutputOffsetYCmdLine = 0;
                } else {
                    std::cerr << command << ": option \"-f\" requires 2 or 4 arguments" << endl;
                    failed = true;
                }
            } else {
                std::cerr << command << ": option \"-f\" requires 2 or 4 arguments" << endl;
                failed = true;
            }
            OutputSizeGiven = true;
            optionSet.insert(SizeAndPositionOption);
            break;

        case 'g':
            GimpAssociatedAlphaHack = true;
            optionSet.insert(AssociatedAlphaOption);
            break;

        case 'l': // FALLTHROUGH
        case LevelsId:
            if (optarg != NULL && *optarg != 0) {
                std::string levels(optarg);
                boost::algorithm::to_upper(levels);
                if (levels == "AUTO" || levels == "AUTOMATIC") {
                    ExactLevels = 0;
                } else if (levels.find_first_not_of("+-0123456789") != std::string::npos) {
                    std::cerr << command <<
                        ": options \"-l\" or \"--levels\" require an integer argument or \"auto\"" << endl;
                    failed = true;
                } else {
                    std::ostringstream oss;
                    oss <<
                        "cannot use more than " << MAX_PYRAMID_LEVELS <<
                        " pyramid levels; will use at most " << MAX_PYRAMID_LEVELS << " levels";
                    ExactLevels =
                        enblend::numberOfString(optarg,
                                                _1 != 0,
                                                "cannot blend with zero levels; will use one level",
                                                1,
                                                _1 <= MAX_PYRAMID_LEVELS,
                                                oss.str(),
                                                MAX_PYRAMID_LEVELS);
                }
            } else {
                std::cerr << command << ": options \"-l\" or \"--levels\" require an argument" << endl;
                failed = true;
            }
            optionSet.insert(LevelsOption);
            break;

        case 'm':
            if (optarg != NULL && *optarg != 0) {
                const int cache_size =
                    enblend::numberOfString(optarg,
                                            _1 >= 1, //< src::minimum-cache-size 1@dmn{MB}
                                            "cache memory limit less than 1 MB; will use 1 MB",
                                            1);
                vigra_ext::CachedFileImageDirector::v().setAllocation(static_cast<long long>(cache_size) << 20);
            } else {
                std::cerr << command << ": option \"-m\" requires an argument" << endl;
                failed = true;
            }
            optionSet.insert(CacheSizeOption);
            break;

        case LayerSelectorId: {
            selector::algorithm_list::const_iterator selector = selector::find_by_name(optarg);
            if (selector != selector::algorithms.end()) {
                LayerSelection.set_selector(*selector);
            } else {
                std::cerr << command << ": unknown selector algorithm \"" << optarg << "\"";
                exit(1);
            }
            optionSet.insert(LayerSelectorOption);
            break;
        }

        case ParameterId: {
            boost::scoped_ptr<char> s(new char[strlen(optarg) + 1]);
            strcpy(s.get(), optarg);
            char* save_ptr = NULL;
            char* token = strtok_r(s.get(), NUMERIC_OPTION_DELIMITERS, &save_ptr);

            while (token != NULL) {
                std::string key;
                std::string value;
                char* delimiter = strpbrk(token, ASSIGNMENT_CHARACTERS);

                if (delimiter == NULL) {
                    key = token;
                } else {
                    key = std::string(token, delimiter);
                    value = delimiter + 1;
                }
                boost::trim(key);
                boost::trim(value);

                if (enblend::parameter::is_valid_identifier(key)) {
                    Parameter.insert(parameter_map::value_type(key, ParameterValue(value)));
                } else {
                    std::cerr << command << ": warning: key \"" << key << "\" of pair \"" << token <<
                        "\" is not a valid identifier; ignoring\n";
                }

                token = strtok_r(NULL, NUMERIC_OPTION_DELIMITERS, &save_ptr);
            }

            break;
        }

        case NoParameterId: {
            boost::scoped_ptr<char> s(new char[strlen(optarg) + 1]);
            strcpy(s.get(), optarg);
            char* save_ptr = NULL;
            char* token = strtok_r(s.get(), NUMERIC_OPTION_DELIMITERS, &save_ptr);

            while (token != NULL) {
                std::string key(token);
                boost::trim(key);

                if (key == "*") {
                    Parameter.clear();
                } else if (enblend::parameter::is_valid_identifier(key)) {
                    Parameter.erase(key);
                } else {
                    std::cerr << command << ": warning: key \"" << key <<
                        "\" is not a valid identifier; ignoring\n";
                }

                token = strtok_r(NULL, NUMERIC_OPTION_DELIMITERS, &save_ptr);
            }

            break;
        }

        case '?':
            switch (optopt) {
            case 0: // unknown long option
                std::cerr << command << ": unknown option \"" << argv[optind - 1] << "\"\n";
                break;
            case 'b':           // FALLTHROUGH
            case 'd':           // FALLTHROUGH
            case 'f':           // FALLTHROUGH
            case 'l':           // FALLTHROUGH
            case 'm':           // FALLTHROUGH
            case 'o':
                std::cerr << command
                          << ": option \"-" << static_cast<char>(optopt) << "\" requires an argument"
                          << endl;
                break;

            default:
                std::cerr << command << ": unknown option ";
                if (isprint(optopt)) {
                    std::cerr << "\"-" << static_cast<char>(optopt) << "\"";
                } else {
                    std::cerr << "character 0x" << std::hex << optopt;
                }
                std::cerr << endl;
            }
            std::cerr << "Try \"enfuse --help\" for more information." << endl;
            exit(1);

        default:
            std::cerr << command
                      << ": internal error: unhandled command line option"
                      << endl;
            exit(1);
        }
    }

    if (contains(optionSet, SaveMasksOption) && contains(optionSet, LoadMasksOption))
    {
        std::cerr << command
                  << ": options \"--load-masks\" and \"--save-masks\" are mutually exclusive" << endl;
        failed = true;
    }

    if (failed) {
        exit(1);
    }

    if (justPrintUsage) {
        printUsageAndExit(false);
        // never reached
    }

    if (justPrintVersion) {
        printVersionAndExit();
        // never reached
    }

    StopAfterMaskGeneration = contains(optionSet, SaveMasksOption) && !contains(optionSet, OutputOption);

    warn_of_ineffective_options(optionSet);

    return optind;
}


int main(int argc, char** argv)
{
#ifdef _MSC_VER
    // Make sure the FPU is set to rounding mode so that the lrint
    // functions in float_cast.h will work properly.
    // See changes in vigra numerictraits.hxx
    _controlfp(_RC_NEAR, _MCW_RC);
#else
    fesetround(FE_TONEAREST);
#endif

#ifndef _WIN32
    sigemptyset(&SigintMask);
    sigaddset(&SigintMask, SIGINT);

    struct sigaction action;
    action.sa_handler = sigint_handler;
    sigemptyset(&(action.sa_mask));
    sigaction(SIGINT, &action, NULL);
#else
    signal(SIGINT, sigint_handler);
#endif

    if (atexit(cleanup_output) != 0) {
        std::cerr << command << ": warning: could not install cleanup routine\n";
    }

    sig.initialize();

    gsl_set_error_handler_off();

    TIFFSetWarningHandler(tiff_warning);
    TIFFSetErrorHandler(tiff_error);

    //< src::layer-selector all-layers
    LayerSelection.set_selector(*selector::find_by_id(selector::AllLayersId));

    if (!getopt_long_works_ok())
    {
        std::cerr << command << ": cannot reliably parse command line; giving up\n";
        exit(1);
    }

    int optind;
    try {
        optind = process_options(argc, argv);
    } catch (vigra::StdException& e) {
        std::cerr << command << ": error while processing command line options\n"
                  << command << ":     " << e.what()
                  << endl;
        exit(1);
    }

    enblend::TraceableFileNameList inputTraceableFileNameList;

    // Remaining parameters are input files.
    while (optind < argc) {
        enblend::TraceableFileNameList files;
        enblend::unfold_filename(files, std::string(argv[optind]));
        inputTraceableFileNameList.insert(inputTraceableFileNameList.end(),
                                          files.begin(), files.end());
        optind++;
    }

    if (inputTraceableFileNameList.empty()) {
        std::cerr << command << ": no input files specified\n";
        exit(1);
    }

#ifdef DEBUG_DUMP_GLOBAL_VARIABLES
    DUMP_GLOBAL_VARIABLES();
#endif

    sig.check();

    for (enblend::TraceableFileNameList::iterator i = inputTraceableFileNameList.begin();
         i != inputTraceableFileNameList.end();
         ++i) {
        if (!enblend::can_open_file(i->filename())) {
            i->unroll_trace();
            exit(1);
        }
    }

    LayerSelection.retrieve_image_information(inputTraceableFileNameList.begin(),
                                              inputTraceableFileNameList.end());

    // List of info structures for each input image.
    std::list<vigra::ImageImportInfo*> imageInfoList;
    std::list<vigra::ImageImportInfo*>::iterator imageInfoIterator;

    bool isColor = false;
    std::string pixelType;
    TiffResolution resolution;
    vigra::ImageImportInfo::ICCProfile iccProfile;
    vigra::Rect2D inputUnion;

    // Check that all input images have the same parameters.
    unsigned layer = 0;
    unsigned layers = 0;
    enblend::FileNameList inputFileNameList;
    enblend::TraceableFileNameList::iterator inputFileNameIterator = inputTraceableFileNameList.begin();
    while (inputFileNameIterator != inputTraceableFileNameList.end()) {
        vigra::ImageImportInfo* inputInfo = NULL;
        std::string filename(inputFileNameIterator->filename());
        try {
            vigra::ImageImportInfo info(filename.c_str());
            if (layers == 0) { // OPTIMIZATION: call only once per file
                layers = info.numImages();
            }
            inputInfo = new vigra::ImageImportInfo(info);
            inputInfo->setImageIndex(layer);
            ++layer;
        } catch (vigra::ContractViolation& exception) {
            std::cerr <<
                command << ": cannot load image \"" << filename << "\"\n" <<
                command << ":     " << exception.what() << "\n";
            if (enblend::maybe_response_file(filename)) {
                std::cerr <<
                    command << ": info: Maybe you meant a response file and forgot the initial '" <<
                    RESPONSE_FILE_PREFIX_CHAR << "'?\n";
            }
            exit(1);
        }

        LayerSelection.set_selector(inputFileNameIterator->selector());
        if (LayerSelection.accept(filename, layer)) {
            if (Verbose >= VERBOSE_LAYER_SELECTION) {
                std::cerr << command << ": info: layer selector \"" << LayerSelection.name() << "\" accepts\n"
                          << command << ": info:     layer " << layer << " of " << layers << " in image \""
                          << filename << "\"\n";
            }

            // Save this image info in the list.
            imageInfoList.push_back(inputInfo);
            inputFileNameList.push_back(filename);

            if (Verbose >= VERBOSE_INPUT_IMAGE_INFO_MESSAGES) {
                std::cerr << command
                          << ": info: input image \""
                          << inputFileNameIterator->filename()
                          << "\" "
                          << layer << '/' << layers << ' ';

                if (inputInfo->isColor()) {
                    std::cerr << "RGB ";
                }

                if (!inputInfo->getICCProfile().empty()) {
                    std::cerr << "ICC ";
                }

                std::cerr << inputInfo->getPixelType()
                          << " position="
                          << inputInfo->getPosition().x
                          << "x"
                          << inputInfo->getPosition().y
                          << " "
                          << "size="
                          << inputInfo->width()
                          << "x"
                          << inputInfo->height()
                          << endl;
            }

            if (inputInfo->numExtraBands() < 1) {
                // Complain about lack of alpha channel.
                std::cerr << command
                          << ": info: input image \"" << inputFileNameIterator->filename() << "\""
                          << enblend::optional_layer_name(layer, layers)
                          << " does not have an alpha channel;\n";
                inputFileNameIterator->unroll_trace();
                std::cerr << command
                          << ": info: assuming all pixels should contribute to the final image"
                          << endl;
            }

            // Get input image's position and size.
            vigra::Rect2D imageROI(vigra::Point2D(inputInfo->getPosition()),
                                   vigra::Size2D(inputInfo->width(), inputInfo->height()));

            if (inputFileNameIterator == inputTraceableFileNameList.begin()) {
                // First input image
                inputUnion = imageROI;
                isColor = inputInfo->isColor();
                pixelType = inputInfo->getPixelType();
                resolution = TiffResolution(inputInfo->getXResolution(),
                                            inputInfo->getYResolution());
                iccProfile = inputInfo->getICCProfile();
                if (!iccProfile.empty()) {
                    InputProfile = cmsOpenProfileFromMem(iccProfile.data(), iccProfile.size());
                    if (InputProfile == NULL) {
                        std::cerr << endl
                                  << command << ": error parsing ICC profile data from file \""
                                  << inputFileNameIterator->filename()
                                  << "\"" << enblend::optional_layer_name(layer, layers) << endl;
                        inputFileNameIterator->unroll_trace();
                        exit(1);
                    }
                }
            } else {
                // Second and later images
                inputUnion |= imageROI;

                if (isColor != inputInfo->isColor()) {
                    std::cerr << command << ": input image \""
                              << inputFileNameIterator->filename() << "\""
                              << enblend::optional_layer_name(layer, layers) << " is "
                              << (inputInfo->isColor() ? "color" : "grayscale") << "\n"
                              << command << ":   but previous images are "
                              << (isColor ? "color" : "grayscale")
                              << endl;
                    inputFileNameIterator->unroll_trace();
                    exit(1);
                }
                if (pixelType != inputInfo->getPixelType()) {
                    std::cerr << command << ": input image \""
                              << inputFileNameIterator->filename() << "\""
                              << enblend::optional_layer_name(layer, layers) << " has pixel type "
                              << inputInfo->getPixelType() << ",\n"
                              << command << ":   but previous images have pixel type "
                              << pixelType
                              << endl;
                    inputFileNameIterator->unroll_trace();
                    exit(1);
                }
                if (resolution !=
                    TiffResolution(inputInfo->getXResolution(), inputInfo->getYResolution())) {
                    std::cerr << command << ": info: input image \""
                              << inputFileNameIterator->filename() << "\""
                              << enblend::optional_layer_name(layer, layers) << " has resolution "
                              << inputInfo->getXResolution() << " dpi x "
                              << inputInfo->getYResolution() << " dpi,\n"
                              << command << ": info:   but first image has resolution "
                              << resolution.x << " dpi x " << resolution.y << " dpi"
                              << endl;
                    inputFileNameIterator->unroll_trace();
                }
                // IMPLEMENTATION NOTE: Newer Vigra libraries have
                // ICCProfile::operator==.  We substitute STL's equal
                // function plus some extra checks for the case of empty
                // profiles.  -- cls @ Thu May 27 14:21:57 UTC 2010
                if (iccProfile.empty() != inputInfo->getICCProfile().empty() ||
                    !std::equal(iccProfile.begin(), iccProfile.end(),
                                inputInfo->getICCProfile().begin())) {
                    vigra::ImageImportInfo::ICCProfile mismatchProfile = inputInfo->getICCProfile();
                    cmsHPROFILE newProfile = NULL;
                    if (!mismatchProfile.empty()) {
                        newProfile = cmsOpenProfileFromMem(mismatchProfile.data(),
                                                           mismatchProfile.size());
                        if (newProfile == NULL) {
                            std::cerr << endl
                                      << command << ": error parsing ICC profile data from file \""
                                      << inputFileNameIterator->filename()
                                      << "\"" << enblend::optional_layer_name(layer, layers) << endl;
                            inputFileNameIterator->unroll_trace();
                            exit(1);
                        }
                    }

                    std::cerr << endl << command << ": warning: input image \""
                              << inputFileNameIterator->filename()
                              << "\"" << enblend::optional_layer_name(layer, layers) << "\n";
                    inputFileNameIterator->unroll_trace();
                    std::cerr << command << ": warning: has ";
                    if (newProfile) {
                        std::cerr << "ICC profile \"" << enblend::profileDescription(newProfile) << "\",\n";
                    } else {
                        std::cerr << "no ICC profile,\n";
                    }
                    std::cerr << command << ": warning: but first image has ";
                    if (InputProfile) {
                        std::cerr << "ICC profile \"" << enblend::profileDescription(InputProfile) << "\";\n";
                    } else {
                        std::cerr << "no ICC profile;\n";
                    }
                    std::cerr << command << ": warning: blending images with different color spaces\n"
                              << command << ": warning: may have unexpected results"
                              << endl;
                }
            }
        } else {
            if (Verbose >= VERBOSE_LAYER_SELECTION) {
                std::cerr << command << ": info: layer selector \"" << LayerSelection.name() << "\" rejects\n"
                          << command << ": info:     layer " << layer << " of " << layers << " in image \""
                          << filename << "\"\n";
            }
        }

        if (layers == 1 || layer == layers) {
            layer = 0;
            layers = 0;
            ++inputFileNameIterator;
        } else {
            // We are about to process the next layer in the _same_
            // image.  The imageInfoList already has been updated, but
            // inputTraceableFileNameList still lacks the filename.
            inputTraceableFileNameList.insert(inputFileNameIterator, *inputFileNameIterator);
        }
    }

    // Check that more than one input file was given.
    if (imageInfoList.size() <= 1) {
        const size_t n = inputTraceableFileNameList.size();
        const size_t m = imageInfoList.size();

        if (n > m) {
            std::cerr << command << ": warning: selector has rejected " << n - m << " out of " << n << " images\n";
        }

        switch (m) {
        case 0:
            std::cerr << command << ": no input images given\n";
            exit(1);
            break;
        case 1:
            std::cerr << command << ": warning: only one input image given;\n"
                      << command << ": warning: Enfuse needs two or more overlapping input images in order to do\n"
                      << command << ": warning: blending calculations.  The output will be the same as the input.\n";
            break;
        }
    }

    if (resolution == TiffResolution()) {
        std::cerr << command
                  << ": warning: no usable resolution found in first image \""
                  << inputTraceableFileNameList.begin()->filename() << "\";\n"
                  << command
                  << ": warning:   will use " << DEFAULT_TIFF_RESOLUTION << " dpi\n";
        ImageResolution = TiffResolution(DEFAULT_TIFF_RESOLUTION,
                                         DEFAULT_TIFF_RESOLUTION);
    } else {
        ImageResolution = resolution;
    }

    // Create the Info for the output file.
    vigra::ImageExportInfo outputImageInfo(OutputFileName.c_str());

    if (!StopAfterMaskGeneration) {
        OutputIsValid = false;

        // Make sure that inputUnion is at least as big as given by the -f paramater.
        if (OutputSizeGiven) {
            inputUnion |= vigra::Rect2D(OutputOffsetXCmdLine,
                                        OutputOffsetYCmdLine,
                                        OutputOffsetXCmdLine + OutputWidthCmdLine,
                                        OutputOffsetYCmdLine + OutputHeightCmdLine);
        }

        if (!OutputCompression.empty()) {
            outputImageInfo.setCompression(OutputCompression.c_str());
        }

        // If not overridden by the command line, the pixel type of the
        // output image is the same as the input images'.  If the pixel
        // type is not supported by the output format, replace it with the
        // best match.
        {
            const std::string outputFileType = enblend::getFileType(OutputFileName);
            const std::string neededPixelType = OutputPixelType.empty() ? std::string(pixelType) : OutputPixelType;
            const std::string bestPixelType = enblend::bestPixelType(outputFileType, neededPixelType);

            if (neededPixelType != bestPixelType) {
                std::cerr << command
                          << ": warning: "
                          << (OutputPixelType.empty() ? "deduced" : "requested")
                          << " output pixel type is \""
                          << neededPixelType
                          << "\", but image type \""
                          << outputFileType
                          << "\"\n"
                          << command << ": warning:   supports \""
                          << bestPixelType
                          << "\" at best;  will use \""
                          << bestPixelType
                          << "\""
                          << endl;
            }
            outputImageInfo.setPixelType(bestPixelType.c_str());
            pixelType = enblend::maxPixelType(pixelType, bestPixelType);
        }

        // Set the output image ICC profile
        outputImageInfo.setICCProfile(iccProfile);

        if (UseCIECAM == true || (boost::indeterminate(UseCIECAM) && !iccProfile.empty())) {
            UseCIECAM = true;
            if (InputProfile == NULL) {
                std::cerr << command << ": warning: input images do not have ICC profiles;\n";
                if (FallbackProfile == NULL) {
                    std::cerr << command << ": warning: assuming sRGB profile" << endl;
                    InputProfile = cmsCreate_sRGBProfile();
                } else {
                    std::cerr << command << ": warning: using fallback profile \""
                              << enblend::profileDescription(FallbackProfile) << "\"" << endl;
                    InputProfile = FallbackProfile;
                    FallbackProfile = NULL; // avoid double freeing
                }
            }
            XYZProfile = cmsCreateXYZProfile();

            InputToXYZTransform = cmsCreateTransform(InputProfile, TYPE_RGB_DBL,
                                                     XYZProfile, TYPE_XYZ_DBL,
                                                     RENDERING_INTENT_FOR_BLENDING,
                                                     TRANSFORMATION_FLAGS_FOR_BLENDING);
            if (InputToXYZTransform == NULL) {
                std::cerr << command << ": error building color transform from \""
                          << enblend::profileName(InputProfile)
                          << " "
                          << enblend::profileDescription(InputProfile)
                          << "\" to XYZ space" << endl;
                exit(1);
            }

            XYZToInputTransform = cmsCreateTransform(XYZProfile, TYPE_XYZ_DBL,
                                                     InputProfile, TYPE_RGB_DBL,
                                                     RENDERING_INTENT_FOR_BLENDING,
                                                     TRANSFORMATION_FLAGS_FOR_BLENDING);
            if (XYZToInputTransform == NULL) {
                std::cerr << command
                          << ": error building color transform from XYZ space to \""
                          << enblend::profileName(InputProfile)
                          << " "
                          << enblend::profileDescription(InputProfile)
                          << "\"" << endl;
                exit(1);
            }

            // P2 Viewing Conditions: D50, 500 lumens
            ViewingConditions.whitePoint.X = XYZ_SCALE * cmsD50_XYZ()->X;
            ViewingConditions.whitePoint.Y = XYZ_SCALE * cmsD50_XYZ()->Y;
            ViewingConditions.whitePoint.Z = XYZ_SCALE * cmsD50_XYZ()->Z;
            ViewingConditions.Yb = 20.0;
            ViewingConditions.La = 31.83;
            ViewingConditions.surround = AVG_SURROUND;
            ViewingConditions.D_value = 1.0;

            CIECAMTransform = cmsCIECAM02Init(NULL, &ViewingConditions);
            if (!CIECAMTransform) {
                std::cerr << endl
                          << command
                          << ": error initializing CIECAM02 transform"
                          << endl;
                exit(1);
            }

            cmsCIExyY white_point;
            if (cmsIsTag(InputProfile, cmsSigMediaWhitePointTag)) {
                cmsXYZ2xyY(&white_point,
                           (const cmsCIEXYZ*) cmsReadTag(InputProfile, cmsSigMediaWhitePointTag));
                if (Verbose >= VERBOSE_COLOR_CONVERSION_MESSAGES) {
                    double temperature;
                    cmsTempFromWhitePoint(&temperature, &white_point);
                    std::cerr << command
                              << ": info: using white point of input profile at " << temperature << "K"
                              << endl;
                }
            } else {
                memcpy(&white_point, cmsD50_xyY(), sizeof(cmsCIExyY));
                if (Verbose >= VERBOSE_COLOR_CONVERSION_MESSAGES) {
                    double temperature;
                    cmsTempFromWhitePoint(&temperature, &white_point);
                    std::cerr << command
                              << ": info: falling back to predefined (D50) white point at " << temperature << "K"
                              << endl;
                }
            }
            LabProfile = cmsCreateLab2Profile(cmsD50_xyY());
            InputToLabTransform = cmsCreateTransform(InputProfile, TYPE_RGB_DBL,
                                                     LabProfile, TYPE_Lab_DBL,
                                                     RENDERING_INTENT_FOR_BLENDING,
                                                     TRANSFORMATION_FLAGS_FOR_BLENDING);
            if (!InputToLabTransform) {
                std::cerr << command << ": error building color transform from \""
                          << enblend::profileName(InputProfile)
                          << " "
                          << enblend::profileDescription(InputProfile)
                          << "\" to Lab space" << endl;
                exit(1);
            }
            LabToInputTransform = cmsCreateTransform(LabProfile, TYPE_Lab_DBL,
                                                     InputProfile, TYPE_RGB_DBL,
                                                     RENDERING_INTENT_FOR_BLENDING,
                                                     TRANSFORMATION_FLAGS_FOR_BLENDING);
            if (!LabToInputTransform) {
                std::cerr << command
                          << ": error building color transform from Lab space to \""
                          << enblend::profileName(InputProfile)
                          << " "
                          << enblend::profileDescription(InputProfile)
                          << "\"" << endl;
                exit(1);
            }
        } else {
            if (FallbackProfile != NULL) {
                std::cerr << command <<
                    ": warning: fusing in RGB cube; option \"--fallback-profile\" has no effect" <<
                    endl;
            }
        }

        // The size of the output image.
        if (Verbose >= VERBOSE_INPUT_UNION_SIZE_MESSAGES) {
            std::cerr << command
                      << ": info: output image size: "
                      << inputUnion
                      << endl;
        }

        // Set the output image position and resolution.
        outputImageInfo.setXResolution(ImageResolution.x);
        outputImageInfo.setYResolution(ImageResolution.y);
        outputImageInfo.setPosition(inputUnion.upperLeft());

        // Sanity check on the output image file.
        try {
            // This seems to be a reasonable way to check if
            // the output file is going to work after blending
            // is done.
            encoder(outputImageInfo);
        } catch (vigra::StdException& e) {
            std::cerr << endl
                      << command
                      << ": error opening output file \""
                      << OutputFileName
                      << "\";\n"
                      << command
                      << ": "
                      << e.what()
                      << endl;
            exit(1);
        }

        if (!OutputPixelType.empty()) {
            pixelType = enblend::maxPixelType(pixelType, OutputPixelType);
        }
    }

    // Invoke templatized blender.
    try {
        if (isColor) {
            if      (pixelType == "UINT8")  enblend::enfuseMain<vigra::RGBValue<vigra::UInt8 > >(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
#ifndef DEBUG_8BIT_ONLY
            else if (pixelType == "INT8")   enblend::enfuseMain<vigra::RGBValue<vigra::Int8  > >(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
            else if (pixelType == "UINT16") enblend::enfuseMain<vigra::RGBValue<vigra::UInt16> >(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
            else if (pixelType == "INT16")  enblend::enfuseMain<vigra::RGBValue<vigra::Int16 > >(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
            else if (pixelType == "UINT32") enblend::enfuseMain<vigra::RGBValue<vigra::UInt32> >(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
            else if (pixelType == "INT32")  enblend::enfuseMain<vigra::RGBValue<vigra::Int32 > >(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
            else if (pixelType == "FLOAT")  enblend::enfuseMain<vigra::RGBValue<float > >(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
            else if (pixelType == "DOUBLE") enblend::enfuseMain<vigra::RGBValue<double> >(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
#endif
            else {
                std::cerr << command << ": RGB images with pixel type \""
                          << pixelType
                          << "\" are not supported"
                          << endl;
                exit(1);
            }
        } else {
            if (!WSaturationIsDefault && (WSaturation != 0.0)) {
                std::cerr << command
                          << ": warning: \"--WSaturation\" is not applicable to grayscale images;\n"
                          << command
                          << ": warning:   this parameter will have no effect"
                          << endl;
                WSaturation = 0.0;
            }
            if      (pixelType == "UINT8")  enblend::enfuseMain<vigra::UInt8 >(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
#ifndef DEBUG_8BIT_ONLY
            else if (pixelType == "INT8")   enblend::enfuseMain<vigra::Int8  >(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
            else if (pixelType == "UINT16") enblend::enfuseMain<vigra::UInt16>(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
            else if (pixelType == "INT16")  enblend::enfuseMain<vigra::Int16 >(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
            else if (pixelType == "UINT32") enblend::enfuseMain<vigra::UInt32>(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
            else if (pixelType == "INT32")  enblend::enfuseMain<vigra::Int32 >(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
            else if (pixelType == "FLOAT")  enblend::enfuseMain<float >(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
            else if (pixelType == "DOUBLE") enblend::enfuseMain<double>(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
#endif
            else {
                std::cerr << command
                          << ": black&white images with pixel type \""
                          << pixelType
                          << "\" are not supported"
                          << endl;
                exit(1);
            }
        }

        for (std::list<vigra::ImageImportInfo*>::iterator i = imageInfoList.begin();
             i != imageInfoList.end();
             ++i) {
            delete *i;
        }
    } catch (std::bad_alloc& e) {
        std::cerr << endl
                  << command << ": out of memory\n"
                  << command << ": " << e.what()
                  << endl;
        exit(1);
    } catch (vigra::StdException& e) {
        std::cerr << endl
                  << command << ": an exception occured\n"
                  << command << ": " << e.what()
                  << endl;
        exit(1);
    }

    if (FallbackProfile) {cmsCloseProfile(FallbackProfile);}
    if (LabProfile) {cmsCloseProfile(LabProfile);}
    if (InputToLabTransform) {cmsCIECAM02Done(InputToLabTransform);}
    if (LabToInputTransform) {cmsCIECAM02Done(LabToInputTransform);}
    if (CIECAMTransform) {cmsCIECAM02Done(CIECAMTransform);}
    if (InputToXYZTransform) {cmsDeleteTransform(InputToXYZTransform);}
    if (XYZToInputTransform) {cmsDeleteTransform(XYZToInputTransform);}
    if (XYZProfile) {cmsCloseProfile(XYZProfile);}
    if (InputProfile) {cmsCloseProfile(InputProfile);}

    // Success.
    return 0;
}
