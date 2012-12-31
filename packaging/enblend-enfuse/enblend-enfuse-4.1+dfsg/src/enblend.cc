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

#include <boost/algorithm/string/erase.hpp>
#include <boost/logic/tribool.hpp>
#include <boost/random/mersenne_twister.hpp>
#include <lcms2.h>

#include "global.h"
#include "layer_selection.h"
#include "signature.h"
#include "selector.h"
#include "self_test.h"
#include "tiff_message.h"


typedef enum {
    UnknownDifference,
    HueLuminanceMaxDifference,  // maximum of hue difference and luminance difference
    DeltaEDifference            // L*a*b*-based Delta E
} difference_functor_t;


typedef struct {
    unsigned int kmax;          // maximum number of moves for a line segment
    double tau;                 // temperature reduction factor, "cooling factor"; 0 < tau < 1
    double deltaEMax;           // maximum cost change possible by any single annealing move
    double deltaEMin;           // minimum cost change possible by any single annealing move
} anneal_para_t;


// Globals
const std::string command("enblend");
const int minimumVectorizeDistance = 4; //< src::minimum-vectorize-distance 4
const int coarseMaskVectorizeDistance = 4; //< src::coarse-mask-vectorize-distance 4
const int fineMaskVectorizeDistance = 20; //< src::fine-mask-vectorize-distance 20

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
MainAlgo MainAlgorithm = NFT;
bool Checkpoint = false;
bool UseGPU = false;
bool OptimizeMask = true;
bool CoarseMask = true;
unsigned CoarsenessFactor = 8U; //< src::default-coarseness-factor 8
difference_functor_t PixelDifferenceFunctor = DeltaEDifference; //< src::default-difference-functor Delta-E
double LuminanceDifferenceWeight = 1.0; //< src::default-luminance-difference-weight 1.0
double ChrominanceDifferenceWeight = 1.0; //< src::default-chrominance-difference-weight 1.0
double DifferenceBlurRadius = 0.0;
bool SaveMasks = false;
bool StopAfterMaskGeneration = false;
std::string SaveMaskTemplate("mask-%n.tif"); //< src::default-mask-template mask-%n.tif
bool LoadMasks = false;
std::string LoadMaskTemplate(SaveMaskTemplate);
std::string VisualizeTemplate("vis-%n.tif"); //< src::default-visualize-template vis-%n.tif
bool VisualizeSeam = false;
std::pair<double, double> OptimizerWeights =
    std::make_pair(8.0,      //< src::default-optimizer-weight-distance 8.0
                   1.0);     //< src::default-optimizer-weight-mismatch 1.0
anneal_para_t AnnealPara = {
    32,                         //< src::default-anneal-kmax 32
    0.75,                       //< src::default-anneal-tau 0.75
    7000.0,                     //< src::default-anneal-deltae-max 7000.0
    5.0                         //< src::default-anneal-deltae-min 5.0
};
unsigned int DijkstraRadius = 25U; //< src::default-dijkstra-radius 25
AlternativePercentage MaskVectorizeDistance(0.0, false);
std::string OutputCompression;
std::string OutputPixelType;

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
#include "filespec.h"
#include "enblend.h"
#ifdef HAVE_LIBGLEW
#include "gpu.h"
#endif

#ifdef DMALLOC
#include "dmalloc.h"            // must be last #include
#endif

#ifdef _WIN32
#define strdup _strdup
#endif


difference_functor_t
differenceFunctorOfString(const char* aDifferenceFunctorName)
{
    std::string name(aDifferenceFunctorName);

    boost::algorithm::erase_all(name, "-");
    boost::algorithm::to_lower(name);

    if (name == "maximumhueluminance" || name == "maximumhuelum" ||
        name == "maxhueluminance" || name == "maxhuelum" || name == "max") {
        return HueLuminanceMaxDifference;
    } else if (name == "deltae" || name == "de") {
        return DeltaEDifference;
    } else {
        return UnknownDifference;
    }
}


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
        "+ Checkpoint = " << enblend::stringOfBool(Checkpoint) << ", option \"-x\"\n" <<
        "+ UseGPU = " << enblend::stringOfBool(UseGPU) << ", option \"--gpu\"\n" <<
        "+ OptimizeMask = " << enblend::stringOfBool(OptimizeMask) <<
        ", options \"--optimize\" and \"--no-optimize\"\n" <<
        "+ CoarseMask = " << enblend::stringOfBool(CoarseMask) <<
        ", options \"--coarse-mask\" and \"--fine-mask\"\n" <<
        "+     CoarsenessFactor = " << CoarsenessFactor << ", argument to option \"--coarse-mask\"\n" <<
        "+ PixelDifferenceFunctor = " << stringOfPixelDifferenceFunctor(PixelDifferenceFunctor) <<
        "+     LuminanceDifferenceWeight = " << LuminanceDifferenceWeight << "\n" <<
        "+     ChrominanceDifferenceWeight = " << ChrominanceDifferenceWeight <<
        ", option \"--image-difference\"\n" <<
        "+ DifferenceBlurRadius = " << DifferenceBlurRadius << ", option \"--smooth-difference\"\n" <<
        "+ SaveMasks = " << enblend::stringOfBool(SaveMasks) << ", option \"--save-masks\"\n" <<
        "+     SaveMaskTemplate = <" << SaveMaskTemplate << ">, argument to option \"--save-masks\"\n" <<
        "+ LoadMasks = " << enblend::stringOfBool(LoadMasks) << ", option \"--load-masks\"\n" <<
        "+     LoadMaskTemplate = <" << LoadMaskTemplate << ">, argument to option \"--load-masks\"\n" <<
        "+ VisualizeSeam = " << enblend::stringOfBool(VisualizeSeam) << ", option \"--visualize\"\n" <<
        "+     VisualizeTemplate = <" << VisualizeTemplate << ">, argument to option \"--visualize\"\n" <<
        "+ OptimizerWeights = {\n" <<
        "+     distance = " << OptimizerWeights.first << ",\n" <<
        "+     mismatch = " << OptimizerWeights.second << "\n" <<
        "+ }, arguments to option \"--visualize\"\n"
        "+ AnnealPara = {\n" <<
        "+     kmax = " << AnnealPara.kmax << ",\n" <<
        "+     tau = " << AnnealPara.tau << ",\n" <<
        "+     deltaEMax = " << AnnealPara.deltaEMax << ",\n" <<
        "+     deltaEMin = " << AnnealPara.deltaEMin << "\n" <<
        "+ }, arguments to option \"--anneal\"\n" <<
        "+ DijkstraRadius = " << DijkstraRadius << ", option \"--dijkstra\"\n" <<
        "+ MaskVectorizeDistance = {\n" <<
        "+     value = " << MaskVectorizeDistance.value() << ",\n" <<
        "+     is_percentage = " << enblend::stringOfBool(MaskVectorizeDistance.is_percentage()) << "\n" <<
        "+ }, arguments to option \"--mask-vectorize\"\n" <<
        "+ OutputCompression = <" << OutputCompression << ">, option \"--compression\"\n" <<
        "+ OutputPixelType = <" << OutputPixelType << ">, option \"--depth\"\n" <<
        "+ end of global variable dump\n";
}


#ifdef HAVE_LIBGLEW
void inspectGPU(int argc, char** argv)
{
#ifdef HAVE_APPLE_OPENGL_FRAMEWORK
    CGLContextObj cgl_context = cgl_init();
#else
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_RGBA);

    const int handle = glutCreateWindow("Enblend");

    if (!(handle >= 1 && glutGet(GLUT_DISPLAY_MODE_POSSIBLE))) {
        std::cout << "    <no reliable OpenGL information available>\n";
        return;
    }
#endif

    std::cout <<
        "  - " << GLGETSTRING(GL_VENDOR) << "\n" <<
        "  - " << GLGETSTRING(GL_RENDERER) << "\n" <<
        "  - version " << GLGETSTRING(GL_VERSION) << "\n"
        "  - extensions\n";

    const char* const extensions = GLGETSTRING(GL_EXTENSIONS);
    const char* const extensions_end = extensions + strlen(extensions);
    const unsigned extensions_per_line = 3U;
    unsigned count = 1U;

    std::cout << "    ";
    for (const char* c = extensions; c != extensions_end; ++c) {
        if (*c == ' ') {
            if (count % extensions_per_line == 0U) {
                std::cout << "\n    ";
            } else {
                std::cout << "  ";
            }
            ++count;
        } else {
            std::cout << *c;
        }
    }
    std::cout << "\n\n";

#ifdef HAVE_APPLE_OPENGL_FRAMEWORK
    CGLDestroyContext(cgl_context);
#else
    glutDestroyWindow(handle);
#endif
}
#endif // HAVE_LIBGLEW


/** Print information on the current version and some configuration
 * details. */
void printVersionAndExit(int argc, char** argv) {
    std::cout << "enblend " << VERSION << "\n\n";

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

#ifdef HAVE_LIBGLEW
        std::cout << "Extra feature: GPU acceleration: yes\n";
        inspectGPU(argc, argv);
#else
        std::cout << "Extra feature: GPU acceleration: no\n";
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
        std::endl;

    exit(0);
}


/** Print the usage information and quit. */
void printUsageAndExit(const bool error = true) {
    std::cout <<
        "Usage: enblend [options] [--output=IMAGE] INPUT...\n" <<
        "Blend INPUT images into a single IMAGE.\n" <<
        "\n" <<
        "INPUT... are image filenames or response filenames.  Response\n" <<
        "filenames start with an \"" << RESPONSE_FILE_PREFIX_CHAR << "\" character.\n"
        "\n" <<
        "Common options:\n" <<
        "  -V, --version          output version information and exit\n" <<
        "  -a                     pre-assemble non-overlapping images\n" <<
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
        "  -x                     checkpoint partial results\n" <<
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
        "  --gpu                  use graphics card to accelerate seam-line optimization\n" <<
        "  -f WIDTHxHEIGHT[+xXOFFSET+yYOFFSET]\n" <<
        "                         manually set the size and position of the output\n" <<
        "                         image; useful for cropped and shifted input\n" <<
        "                         TIFF images, such as those produced by Nona\n" <<
        "  -m CACHESIZE           set image CACHESIZE in megabytes; default: " <<
        (vigra_ext::CachedFileImageDirector::v().getAllocation() / 1048576LL) << "MB\n" <<
        "\n" <<
        "Mask generation options:\n" <<
        "  --primary-seam-generator=ALGORITHM\n" <<
        "                         use main seam finder ALGORITHM, where ALGORITHM is\n"<<
        "                         \"nearest-feature-transform\" or \"graph-cut\";\n" <<
        "                         default: \"nearest-feature-transform\"\n" <<
        "  --image-difference=ALGORITHM[:LUMINANCE-WEIGHT[:CHROMINANCE-WEIGHT]]\n" <<
        "                         use ALGORITHM for calculation of the difference image,\n" <<
        "                         where ALGORITHM is \"max-hue-luminance\" or \"delta-e\";\n" <<
        "                         LUMINANCE-WEIGHT and CHROMINANCE-WEIGHT define the weights\n" <<
        "                         of lightness and color; default: " <<
        stringOfPixelDifferenceFunctor(PixelDifferenceFunctor) << ":" << LuminanceDifferenceWeight <<
        ": " << ChrominanceDifferenceWeight << "\n" <<
        "  --coarse-mask[=FACTOR] shrink overlap regions by FACTOR to speedup mask\n" <<
        "                         generation; this is the default; if omitted FACTOR\n" <<
        "                         defaults to " <<
        CoarsenessFactor << "\n" <<
        "  --fine-mask            generate mask at full image resolution; use e.g.\n" <<
        "                         if overlap regions are very narrow\n" <<
        "  --smooth-difference=RADIUS    (deprecated)\n" <<
        "                         smooth the difference image prior to seam-line\n" <<
        "                         optimization with a Gaussian blur of RADIUS;\n" <<
        "                         default: " << DifferenceBlurRadius << " pixels\n" <<
        "  --optimize             turn on mask optimization; this is the default\n" <<
        "  --no-optimize          turn off mask optimization\n" <<
        "  --optimizer-weights=DISTANCE-WEIGHT[:MISMATCH-WEIGHT]\n" <<
        "                         set the optimizer's weigths for distance and mismatch;\n" <<
        "                         default: " << OptimizerWeights.first << ':' <<
        OptimizerWeights.second << "\n" <<
        "  --mask-vectorize=LENGTH\n" <<
        "                         set LENGTH of single seam segment; append \"%\" for\n" <<
        "                         relative value; defaults: " <<
        coarseMaskVectorizeDistance << " for coarse masks and\n" <<
        "                         " <<
        fineMaskVectorizeDistance << " for fine masks\n" <<
        "  --anneal=TAU[:DELTAE-MAX[:DELTAE-MIN[:K-MAX]]]\n" <<
        "                         set annealing parameters of optimizer strategy 1;\n" <<
        "                         defaults: " << AnnealPara.tau << ':' <<
        AnnealPara.deltaEMax << ':' << AnnealPara.deltaEMin << ':' << AnnealPara.kmax << "\n" <<
        "  --dijkstra=RADIUS      set search RADIUS of optimizer strategy 2; default:\n" <<
        "                         " << DijkstraRadius << " pixels\n" <<
        "  --save-masks[=TEMPLATE]\n" <<
        "                         save generated masks in TEMPLATE; default: \"" <<
        SaveMaskTemplate << "\";\n" <<
        "                         conversion chars: \"%i\": mask index, \"%n\": mask number,\n" <<
        "                         \"%p\": full path, \"%d\": dirname, \"%b\": basename,\n" <<
        "                         \"%f\": filename, \"%e\": extension; lowercase characters\n" <<
        "                         refer to input images uppercase to the output image\n" <<
        "  --load-masks[=TEMPLATE]\n" <<
        "                         use existing masks in TEMPLATE instead of generating\n" <<
        "                         them; same template characters as \"--save-masks\";\n" <<
        "                         default: \"" << LoadMaskTemplate << "\"\n" <<
        "  --visualize[=TEMPLATE] save results of optimizer in TEMPLATE; same template\n" <<
        "                         characters as \"--save-masks\"; default: \"" <<
        VisualizeTemplate << "\"\n" <<
        "Enblend accepts arguments to any option in uppercase as\n" <<
        "well as in lowercase letters.\n" <<
        "\n" <<
        "Report bugs at <" PACKAGE_BUGREPORT ">." <<
        std::endl;

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
    std::cerr << std::endl << command << ": interrupted" << std::endl;

    cleanup_output();

#ifdef HAVE_LIBGLEW
    if (UseGPU) {
        // FIXME what if this occurs in a GL atomic section?
        wrapupGPU();
    }
#endif

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
    VersionOption, PreAssembleOption /* -a */, HelpOption, LevelsOption,
    OutputOption, VerboseOption, WrapAroundOption /* -w */,
    CheckpointOption /* -x */, CompressionOption, LZWCompressionOption,
    BlockSizeOption, CIECAM02Option, NoCIECAM02Option, FallbackProfileOption,
    DepthOption, AssociatedAlphaOption /* -g */, GPUOption,
    SizeAndPositionOption /* -f */, CacheSizeOption,
    VisualizeOption, CoarseMaskOption, FineMaskOption,
    OptimizeOption, NoOptimizeOption,
    SaveMasksOption, LoadMasksOption,
    ImageDifferenceOption, AnnealOption, DijkstraRadiusOption, MaskVectorizeDistanceOption,
    SmoothDifferenceOption, OptimizerWeightsOption,
    LayerSelectorOption, NearestFeatureTransformOption, GraphCutOption,
    // currently below the radar...
    SequentialBlendingOption
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
        if (contains(optionSet, GPUOption)) {
            std::cerr << command <<
                ": warning: option \"--gpu\" has no effect with \"--load-masks\"" << std::endl;
        }
        if (contains(optionSet, VisualizeOption)) {
            std::cerr << command <<
                ": warning: option \"--visualize\" has no effect with \"--load-masks\"" << std::endl;
        }
        if (contains(optionSet, CoarseMaskOption)) {
            std::cerr << command <<
                ": warning: option \"--coarse-mask\" has no effect with \"--load-masks\"" << std::endl;
        }
        if (contains(optionSet, FineMaskOption)) {
            std::cerr << command <<
                ": warning: option \"--fine-mask\" has no effect with \"--load-masks\"" << std::endl;
        }
        if (contains(optionSet, OptimizeOption)) {
            std::cerr << command <<
                ": warning: option \"--optimize\" has no effect with \"--load-masks\"" << std::endl;
        }
        if (contains(optionSet, NoOptimizeOption)) {
            std::cerr << command <<
                ": warning: option \"--no-optimize\" has no effect with \"--load-masks\"" << std::endl;
        }
        if (contains(optionSet, AnnealOption)) {
            std::cerr << command <<
                ": warning: option \"--anneal\" has no effect with \"--load-masks\"" << std::endl;
        }
        if (contains(optionSet, DijkstraRadiusOption)) {
            std::cerr << command <<
                ": warning: option \"--dijkstra\" has no effect with \"--load-masks\"" << std::endl;
        }
        if (contains(optionSet, MaskVectorizeDistanceOption)) {
            std::cerr << command <<
                ": warning: option \"--mask-vectorize\" has no effect with \"--load-masks\"" << std::endl;
        }
        if (contains(optionSet, SmoothDifferenceOption)) {
            std::cerr << command <<
                ": warning: option \"--smooth-difference\" has no effect with \"--load-masks\"" << std::endl;
        }
        if (contains(optionSet, OptimizerWeightsOption)) {
            std::cerr << command <<
                ": warning: option \"--optimizer-weights\" has no effect with \"--load-masks\"" << std::endl;
        }
    }

    if (contains(optionSet, SaveMasksOption) && !contains(optionSet, OutputOption)) {
        if (contains(optionSet, LevelsOption)) {
            std::cerr << command <<
                ": warning: option \"--levels\" has no effect with \"--save-masks\" and no \"--output\"" <<
                std::endl;
        }
        if (contains(optionSet, WrapAroundOption)) {
            std::cerr << command <<
                ": warning: option \"--wrap\" has no effect with \"--save-masks\" and no \"--output\"" <<
                std::endl;
        }
        if (contains(optionSet, CompressionOption)) {
            std::cerr << command <<
                ": warning: option \"--compression\" has no effect with \"--save-masks\" and no \"--output\"" <<
                std::endl;
        }
        if (contains(optionSet, DepthOption)) {
            std::cerr << command <<
                ": warning: option \"--depth\" has no effect with \"--save-masks\" and no \"--output\"" <<
                std::endl;
        }
        if (contains(optionSet, SizeAndPositionOption)) {
            std::cerr << command <<
                ": warning: option \"-f\" has no effect with \"--save-masks\" and no \"--output\"" << std::endl;
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
            std::endl;
    }

    if (contains(optionSet, AssociatedAlphaOption) &&
        enblend::getFileType(OutputFileName) != "TIFF") {
        std::cerr << command <<
            ": warning: option \"-g\" has no effect with output\n" <<
            command <<
            ": warning:     file type \"" <<
            enblend::getFileType(OutputFileName) << "\"" <<
            std::endl;
    }

    if (!OptimizeMask) {

        if (contains(optionSet, AnnealOption)) {
            std::cerr << command <<
                ": warning: option \"--anneal\" without mask optimization has\n" <<
                command <<
                ": warning:     no effect" <<
                std::endl;
        }

        if (contains(optionSet, DijkstraRadiusOption)) {
            std::cerr << command <<
                ": warning: option \"--dijkstra\" without mask optimization\n" <<
                command <<
                ": warning:     has no effect" <<
                std::endl;
        }

        if (contains(optionSet, SmoothDifferenceOption)) {
            std::cerr << command <<
                ": warning: option \"--smooth-difference\" without mask optimization\n" <<
                command <<
                ": warning:     has no effect" <<
                std::endl;
        }

        if (contains(optionSet, OptimizerWeightsOption)) {
            std::cerr << command <<
                ": warning: option \"--optimizer-weights\" without mask optimization\n" <<
                command <<
                ": warning:     has no effect" <<
                std::endl;
        }
    }

    if (!(OptimizeMask || CoarseMask) && contains(optionSet, MaskVectorizeDistanceOption)) {
        std::cerr << command <<
            ": warning: option \"--mask-vectorize\" without mask optimization\n" <<
            command <<
            ": warning:     or coarse mask has no effect" <<
            std::endl;
    }

    if (!CoarseMask && MainAlgorithm == GraphCut && contains(optionSet, FineMaskOption)) {
        std::cerr << command <<
            ": warning: option \"--fine-mask\" combined with option \"--primary-seam-generator=graphcut\"\n" <<
            command <<
            ": warning:     incompatible with mask optimization,\n" <<
            command <<
            ": warning:     defaulting to no optimization" <<
            std::endl;
        OptimizeMask = false;
    }

#ifndef CACHE_IMAGES
    if (contains(optionSet, CacheSizeOption)) {
        std::cerr << command <<
            ": warning: option \"-m\" has no effect in this " << command << " binary,\n" <<
            command <<
            ": warning:     because it was compiled without image cache" <<
            std::endl;
    }

    if (contains(optionSet, BlockSizeOption)) {
        std::cerr << command <<
            ": warning: option \"-b\" has no effect in this " << command << " binary,\n" <<
            command <<
            ": warning:     because it was compiled without image cache" <<
            std::endl;
    }
#endif
}


int process_options(int argc, char** argv)
{
    enum OptionId {
        OPTION_ID_OFFSET = 1023,    // Ids start at 1024
        UseGpuId,
        CoarseMaskId,
        FineMaskId,
        OptimizeMaskId,
        NoOptimizeMaskId,
        SaveMaskId,
        LoadMaskId,
        VisualizeId,
        AnnealId,
        DijkstraRadiusId,
        MaskVectorizeDistanceId,
        CompressionId,
        VerboseId,
        HelpId,
        VersionId,
        DepthId,
        OutputId,
        WrapAroundId,
        SmoothDifferenceId,
        OptimizerWeightsId,
        LevelsId,
        CiecamId,
        NoCiecamId,
        FallbackProfileId,
        LayerSelectorId,
        MainAlgoId,
        ImageDifferenceId,
        ParameterId,
        NoParameterId
    };

    static struct option long_options[] = {
        {"gpu", no_argument, 0, UseGpuId},
        {"coarse-mask", optional_argument, 0, CoarseMaskId},
        {"fine-mask", no_argument, 0, FineMaskId},
        {"optimize", no_argument, 0, OptimizeMaskId},
        {"no-optimize", no_argument, 0, NoOptimizeMaskId},
        {"save-mask", optional_argument, 0, SaveMaskId}, // singular form: not documented, not deprecated
        {"save-masks", optional_argument, 0, SaveMaskId},
        {"load-mask", optional_argument, 0, LoadMaskId}, // singular form: not documented, not deprecated
        {"load-masks", optional_argument, 0, LoadMaskId},
        {"visualize", optional_argument, 0, VisualizeId},
        {"anneal", required_argument, 0, AnnealId},
        {"dijkstra", required_argument, 0, DijkstraRadiusId},
        {"mask-vectorize", required_argument, 0, MaskVectorizeDistanceId},
        {"compression", required_argument, 0, CompressionId},
        {"verbose", optional_argument, 0, VerboseId},
        {"help", no_argument, 0, HelpId},
        {"version", no_argument, 0, VersionId},
        {"depth", required_argument, 0, DepthId},
        {"output", required_argument, 0, OutputId},
        {"wrap", optional_argument, 0, WrapAroundId},
        {"smooth-difference", required_argument, 0, SmoothDifferenceId},
        {"optimizer-weights", required_argument, 0, OptimizerWeightsId},
        {"levels", required_argument, 0, LevelsId},
        {"ciecam", no_argument, 0, CiecamId},
        {"no-ciecam", no_argument, 0, NoCiecamId},
        {"fallback-profile", required_argument, 0, FallbackProfileId},
        {"layer-selector", required_argument, 0, LayerSelectorId},
        {"primary-seam-generator", required_argument, 0, MainAlgoId},
        {"image-difference", required_argument, 0, ImageDifferenceId},
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
        const int code = getopt_long(argc, argv, "Vab:cd:f:ghl:m:o:sv::w::x",
                                     long_options, &option_index);

        if (code == -1) {
            break;
        }

        switch (code) {
        case UseGpuId:
            UseGPU = true;
            optionSet.insert(GPUOption);
            break;

        case FineMaskId:
            CoarseMask = false;
            optionSet.insert(FineMaskOption);
            break;

        case OptimizeMaskId:
            OptimizeMask = true;
            optionSet.insert(OptimizeOption);
            break;

        case NoOptimizeMaskId:
            OptimizeMask = false;
            optionSet.insert(NoOptimizeOption);
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
                              << ": unrecognized wrap-around mode \"" << optarg << "\"\n" << std::endl;
                    failed = true;
                }
            } else {
                WrapAround = HorizontalStrip;
            }
            optionSet.insert(WrapAroundOption);
            break;

        case SaveMaskId:
            if (optarg != NULL && *optarg != 0) {
                SaveMaskTemplate = optarg;
            }
            SaveMasks = true;
            optionSet.insert(SaveMasksOption);
            break;

        case LoadMaskId:
            if (optarg != NULL && *optarg != 0) {
                LoadMaskTemplate = optarg;
            }
            LoadMasks = true;
            optionSet.insert(LoadMasksOption);
            break;

        case VisualizeId:
            if (optarg != NULL && *optarg != 0) {
                VisualizeTemplate = optarg;
            }
            VisualizeSeam = true;
            optionSet.insert(VisualizeOption);
            break;

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

        case 'd': // FALLTHROUGH
        case DepthId:
            if (optarg != NULL && *optarg != 0) {
                OutputPixelType = enblend::outputPixelTypeOfString(optarg);
            } else {
                std::cerr << command << ": options \"-d\" or \"--depth\" require arguments" << std::endl;
                failed = true;
            }
            optionSet.insert(DepthOption);
            break;

        case 'o': // FALLTHROUGH
        case OutputId:
            if (contains(optionSet, OutputOption)) {
                std::cerr << command
                          << ": warning: more than one output file specified"
                          << std::endl;
            }
            if (optarg != NULL && *optarg != 0) {
                OutputFileName = optarg;
            } else {
                std::cerr << command << ": options \"-o\" or \"--output\" require arguments" << std::endl;
                failed = true;
            }
            optionSet.insert(OutputOption);
            break;

        case ImageDifferenceId: {
            boost::scoped_ptr<char> s(new char[strlen(optarg) + 1]);
            strcpy(s.get(), optarg);
            char* save_ptr = NULL;
            char* token = enblend::strtoken_r(s.get(), NUMERIC_OPTION_DELIMITERS, &save_ptr);
            char* tail;

            if (token == NULL || *token == 0) {
                std::cerr << command << ": option \"--image-difference\" requires an argument" << std::endl;
                failed = true;
            } else {
                PixelDifferenceFunctor = differenceFunctorOfString(token);
                if (PixelDifferenceFunctor == UnknownDifference) {
                    std::cerr << command << ": unknown image difference algorithm \"" << token << "\"" << std::endl;
                    failed = true;
                }
            }

            token = enblend::strtoken_r(NULL, NUMERIC_OPTION_DELIMITERS, &save_ptr);
            if (token != NULL && *token != 0) {
                errno = 0;
                LuminanceDifferenceWeight = strtod(token, &tail);
                if (errno == 0) {
                    if (*tail != 0) {
                        std::cerr << command << ": unrecognized luminance weight \""
                                  << tail << "\" in \"" << token << "\"" << std::endl;
                        failed = true;
                    }
                    if (LuminanceDifferenceWeight < 0.0) {
                        std::cerr << command << ": luminance weight must be non-negative" << std::endl;
                        failed = true;
                    }
                } else {
                    std::cerr << command << ": illegal numeric format \""
                              << token << "\" of luminance weight: "
                              << enblend::errorMessage(errno) << std::endl;
                    failed = true;
                }
            }

            token = enblend::strtoken_r(NULL, NUMERIC_OPTION_DELIMITERS, &save_ptr);
            if (token != NULL && *token != 0) {
                errno = 0;
                ChrominanceDifferenceWeight = strtod(token, &tail);
                if (errno == 0) {
                    if (*tail != 0) {
                        std::cerr << command << ": unrecognized chrominance weight \""
                                  << tail << "\" in \"" << token << "\"" << std::endl;
                        failed = true;
                    }
                    if (ChrominanceDifferenceWeight < 0.0) {
                        std::cerr << command << ": chrominance weight must be non-negative" << std::endl;
                        failed = true;
                    }
                } else {
                    std::cerr << command << ": illegal numeric format \""
                              << token << "\" of chrominance weight: "
                              << enblend::errorMessage(errno) << std::endl;
                    failed = true;
                }
            }

            if (save_ptr != NULL && *save_ptr != 0) {
                std::cerr << command << ": warning: ignoring trailing garbage \""
                          << save_ptr << "\" in argument to \"--image-difference\"" << std::endl;
            }

            if (LuminanceDifferenceWeight + ChrominanceDifferenceWeight == 0.0) {
                std::cerr << command << ": luminance weight and chrominance weight cannot both be zero" << std::endl;
                failed = true;
            }

            optionSet.insert(ImageDifferenceOption);
            break;
        }

        case AnnealId: {
            boost::scoped_ptr<char> s(new char[strlen(optarg) + 1]);
            strcpy(s.get(), optarg);
            char* save_ptr = NULL;
            char* token = enblend::strtoken_r(s.get(), NUMERIC_OPTION_DELIMITERS, &save_ptr);
            char* tail;

            if (token != NULL && *token != 0) {
                errno = 0;
                double tau = strtod(token, &tail);
                if (errno != 0) {
                    std::cerr << command
                              << ": option \"--anneal\": illegal numeric format \""
                              << token << "\" of tau: " << enblend::errorMessage(errno)
                              << std::endl;
                    failed = true;
                }
                if (*tail != 0) {
                    if (*tail == '%') {
                        tau /= 100.0;
                    } else {
                        std::cerr << command
                                  << ": --anneal: trailing garbage \""
                                  << tail << "\" in tau: \"" << token << "\""
                                  << std::endl;
                        failed = true;
                    }
                }
                //< src::minimum-anneal-tau 0
                if (tau <= 0.0) {
                    std::cerr << command
                              << ": option \"--anneal\": tau must be larger than zero"
                              << std::endl;
                    failed = true;
                }
                //< src::maximum-anneal-tau 1
                if (tau >= 1.0) {
                    std::cerr << command
                              << ": option \"--anneal\": tau must be less than one"
                              << std::endl;
                    failed = true;
                }
                AnnealPara.tau = tau;
            }

            token = enblend::strtoken_r(NULL, NUMERIC_OPTION_DELIMITERS, &save_ptr);
            if (token != NULL && *token != 0) {
                errno = 0;
                AnnealPara.deltaEMax = strtod(token, &tail);
                if (errno != 0) {
                    std::cerr << command << ": option \"--anneal\": illegal numeric format \""
                              << token << "\" of deltaE_max: " << enblend::errorMessage(errno)
                              << std::endl;
                    failed = true;
                }
                if (*tail != 0) {
                    std::cerr << command
                              << ": option \"--anneal\": trailing garbage \""
                              << tail << "\" in deltaE_max: \""
                              << token << "\"" << std::endl;
                    failed = true;
                }
                //< src::minimum-anneal-deltae-max 0
                if (AnnealPara.deltaEMax <= 0.0) {
                    std::cerr << command
                              << ": option \"--anneal\": deltaE_max must be larger than zero"
                              << std::endl;
                    failed = true;
                }
            }

            token = enblend::strtoken_r(NULL, NUMERIC_OPTION_DELIMITERS, &save_ptr);
            if (token != NULL && *token != 0) {
                errno = 0;
                AnnealPara.deltaEMin = strtod(token, &tail);
                if (errno != 0) {
                    std::cerr << command
                              << ": option \"--anneal\": illegal numeric format \""
                              << token << "\" of deltaE_min: " << enblend::errorMessage(errno)
                              << std::endl;
                    failed = true;
                }
                if (*tail != 0) {
                    std::cerr << command
                              << ": option \"--anneal\": trailing garbage \""
                              << tail << "\" in deltaE_min: \""
                              << token << "\"" << std::endl;
                    failed = true;
                }
                //< src::minimum-anneal-deltae-min 0
                if (AnnealPara.deltaEMin <= 0.0) {
                    std::cerr << command
                              << ": option \"--anneal\": deltaE_min must be larger than zero"
                              << std::endl;
                    failed = true;
                }
            }
            if (AnnealPara.deltaEMin >= AnnealPara.deltaEMax) {
                std::cerr << command
                          << ": option \"--anneal\": deltaE_min must be less than deltaE_max"
                          << std::endl;
                failed = true;
            }

            token = enblend::strtoken_r(NULL, NUMERIC_OPTION_DELIMITERS, &save_ptr);
            if (token != NULL && *token != 0) {
                errno = 0;
                const long int kmax = strtol(token, &tail, 10);
                if (errno != 0) {
                    std::cerr << command
                              << ": option \"--anneal\": illegal numeric format \""
                              << token << "\" of k_max: " << enblend::errorMessage(errno)
                              << std::endl;
                    failed = true;
                }
                if (*tail != 0) {
                    std::cerr << command
                              << ": option \"--anneal\": trailing garbage \""
                              << tail << "\" in k_max: \""
                              << token << "\"" << std::endl;
                    failed = true;
                }
                //< src::minimum-anneal-kmax 3
                if (kmax < 3L) {
                    std::cerr << command
                              << ": option \"--anneal\": k_max must larger or equal to 3"
                              << std::endl;
                    failed = true;
                }
                AnnealPara.kmax = static_cast<unsigned int>(kmax);
            }

            optionSet.insert(AnnealOption);
            break;
        }

        case MaskVectorizeDistanceId: {
            char* tail;
            MaskVectorizeDistance.set_percentage(false);
            errno = 0;
            MaskVectorizeDistance.set_value(strtod(optarg, &tail));
            if (errno != 0) {
                std::cerr << command
                          << ": option \"--mask-vectorize\": illegal numeric format \""
                          << optarg << "\": " << enblend::errorMessage(errno)
                          << std::endl;
                failed = true;
            }
            if (*tail != 0) {
                if (*tail == '%') {
                    MaskVectorizeDistance.set_percentage(true);
                } else {
                    std::cerr << command
                              << ": option \"--mask-vectorize\": trailing garbage \""
                              << tail << "\" in \"" << optarg << "\"" << std::endl;
                    failed = true;
                }
            }
            if (MaskVectorizeDistance.value() <= 0.0) {
                std::cerr << command
                          << ": option \"--mask-vectorize\": distance must be positive"
                          << std::endl;
                failed = true;
            }

            optionSet.insert(MaskVectorizeDistanceOption);
            break;
        }

        case SmoothDifferenceId: {
            char* tail;
            errno = 0;
            const double radius = strtod(optarg, &tail);
            if (errno != 0) {
                std::cerr << command
                          << ": option \"--smooth-difference\": illegal numeric format \""
                          << optarg << "\": " << enblend::errorMessage(errno)
                          << std::endl;
                failed = true;
            }
            if (*tail != 0) {
                std::cerr << command
                          << ": option \"--smooth-difference\": trailing garbage \""
                          << tail << "\" in \"" << optarg << "\"" << std::endl;
                failed = true;
            }
            //< src::minimum-smooth-difference 0.0
            if (radius < 0.0) {
                std::cerr << command
                          << ": option \"--smooth-difference\": negative radius; will not blur"
                          << std::endl;
                DifferenceBlurRadius = 0.0;
            } else {
                DifferenceBlurRadius = radius;
            }

            optionSet.insert(SmoothDifferenceOption);
            break;
        }

        case OptimizerWeightsId: {
            boost::scoped_ptr<char> s(new char[strlen(optarg) + 1]);
            strcpy(s.get(), optarg);
            char* save_ptr = NULL;
            char* token = enblend::strtoken_r(s.get(), NUMERIC_OPTION_DELIMITERS, &save_ptr);
            OptimizerWeights.first =
                enblend::numberOfString(token,
                                        _1 >= 0.0,
                                        "negative optimizer weight; will use 0.0",
                                        0.0);
            token = enblend::strtoken_r(NULL, NUMERIC_OPTION_DELIMITERS, &save_ptr);
            if (token != NULL && *token != 0) {
                OptimizerWeights.second =
                    enblend::numberOfString(token,
                                            _1 >= 0.0,
                                            "negative optimizer weight; will use 0.0",
                                            0.0);
            }
            if (OptimizerWeights.first == 0.0 && OptimizerWeights.second == 0.0) {
                std::cerr << command
                          << ": optimizer weights cannot be both zero"
                          << std::endl;
            }
            optionSet.insert(OptimizerWeightsOption);
            break;
        }

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

        case CoarseMaskId:
            CoarseMask = true;
            if (optarg != NULL && *optarg != 0) {
                CoarsenessFactor =
                    enblend::numberOfString(optarg,
                                            _1 >= 1U,
                                            "coarseness factor less or equal to 0; will use 1",
                                            1U);
            }
            optionSet.insert(CoarseMaskOption);
            break;

        case DijkstraRadiusId:
            DijkstraRadius =
                enblend::numberOfString(optarg,
                                        _1 >= 1U, //< src::minimum-dijkstra-radius 1
                                        "Dijkstra radius is 0; will use 1",
                                        1U);
            optionSet.insert(DijkstraRadiusOption);
            break;

        case 'a':
            OneAtATime = false;
            optionSet.insert(PreAssembleOption);
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
                std::cerr << command << ": option \"-b\" requires an argument" << std::endl;
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

        case MainAlgoId:
            if (optarg != NULL && *optarg != 0) {
                std::string algo_name(optarg);
                boost::algorithm::to_upper(algo_name);
                if (algo_name == "GRAPH-CUT" ||
                    algo_name == "GRAPHCUT" ||
                    algo_name == "GC") {
                    MainAlgorithm = GraphCut;
                    optionSet.insert(GraphCutOption);
                } else if (algo_name == "NEAREST-FEATURE-TRANSFORM" ||
                           algo_name == "NEARESTFEATURETRANSFORM" ||
                           algo_name == "NFT") {
                    MainAlgorithm = NFT;
                    optionSet.insert(NearestFeatureTransformOption);
                } else {
                    std::cerr << command << ": warning: option \"--primary-seam-generator\": " <<
                        "unrecognized argument \"" << optarg << "\", defaulting to NFT" << std::endl;
                    MainAlgorithm = NFT;
                    optionSet.insert(NearestFeatureTransformOption);
                }
            } else {
                std::cerr << command << ": option \"--primary-seam-generator\" requires an argument" << std::endl;
                failed = true;
            }
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
                    std::cerr << command << ": option \"-f\" requires 2 or 4 arguments" << std::endl;
                    failed = true;
                }
            } else {
                std::cerr << command << ": option \"-f\" requires 2 or 4 arguments" << std::endl;
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
                        ": options \"-l\" or \"--levels\" require an integer argument or \"auto\"" << std::endl;
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
                std::cerr << command << ": options \"-l\" or \"--levels\" require an argument" << std::endl;
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
                std::cerr << command << ": option \"-m\" requires an argument" << std::endl;
                failed = true;
            }
            optionSet.insert(CacheSizeOption);
            break;

        case 's':
            // Deprecated sequential blending flag.
            OneAtATime = true;
            std::cerr << command << ": warning: flag \"-s\" is deprecated." << std::endl;
            optionSet.insert(SequentialBlendingOption);
            break;

        case 'x':
            Checkpoint = true;
            optionSet.insert(CheckpointOption);
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
                          << std::endl;
                break;

            default:
                std::cerr << command << ": unknown option ";
                if (isprint(optopt)) {
                    std::cerr << "\"-" << static_cast<char>(optopt) << "\"";
                } else {
                    std::cerr << "character 0x" << std::hex << optopt;
                }
                std::cerr << std::endl;
            }
            std::cerr << "Try \"enblend --help\" for more information." << std::endl;
            exit(1);

        default:
            std::cerr << command
                      << ": internal error: unhandled command line option"
                      << std::endl;
            exit(1);
        }
    }

    if (contains(optionSet, SaveMasksOption) && contains(optionSet, LoadMasksOption))
    {
        std::cerr << command
                  << ": options \"--load-masks\" and \"--save-masks\" are mutually exclusive" << std::endl;
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
        printVersionAndExit(argc, argv);
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
                  << std::endl;
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

    if (UseGPU) {
#ifdef HAVE_LIBGLEW
        initGPU(&argc, argv);
#else
        std::cerr << command
                  << ": warning: no GPU support compiled in; option \"--gpu\" has no effect"
                  << std::endl;
#endif
    }

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

    //if (vigra_ext::CachedFileImageDirector::v()->getManagedBlocks() < 4) {
    //    // Max simultaneous image access is in:
    //    // 4 in any of many calls to combineThreeImages
    //    // 4 gaussian pyramid init (src image layer, src alpha layer, dest pyramid image layer 0, dest pyramid alpha layer 0)
    //    // 4 in reduce (src image layer N, src alpha layer N, dest image layer N+1, dest alpha layer N+1)
    //    // FIXME complain or automatically adjust blocksize to get ManagedBlocks above 4?
    //}

    // List of info structures for each input image.
    std::list<vigra::ImageImportInfo*> imageInfoList;
    std::list<vigra::ImageImportInfo*>::iterator imageInfoIterator;

    bool isColor = false;
    std::string pixelType;
    TiffResolution resolution;
    vigra::ImageImportInfo::ICCProfile iccProfile;
    vigra::Rect2D inputUnion;

    // Check that all input images have the same parameters.
    int minDim = INT_MAX;
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
                          << std::endl;
            }

            if (inputInfo->numExtraBands() < 1) {
                // Complain about lack of alpha channel.
                std::cerr << command
                          << ": input image \"" << inputFileNameIterator->filename() << "\""
                          << enblend::optional_layer_name(layer, layers)
                          << " does not have an alpha channel\n";
                inputFileNameIterator->unroll_trace();
                exit(1);
            }

            // Get input image's position and size.
            vigra::Rect2D imageROI(vigra::Point2D(inputInfo->getPosition()),
                                   vigra::Size2D(inputInfo->width(), inputInfo->height()));

            if (inputFileNameIterator == inputTraceableFileNameList.begin()) {
                // First input image
                minDim = std::min(inputInfo->width(), inputInfo->height());
                inputUnion = imageROI;
                isColor = inputInfo->isColor();
                pixelType = inputInfo->getPixelType();
                resolution = TiffResolution(inputInfo->getXResolution(),
                                            inputInfo->getYResolution());
                iccProfile = inputInfo->getICCProfile();
                if (!iccProfile.empty()) {
                    InputProfile = cmsOpenProfileFromMem(iccProfile.data(), iccProfile.size());
                    if (InputProfile == NULL) {
                        std::cerr << std::endl
                                  << command << ": error parsing ICC profile data from file \""
                                  << inputFileNameIterator->filename()
                                  << "\"" << enblend::optional_layer_name(layer, layers) << std::endl;
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
                              << std::endl;
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
                              << std::endl;
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
                              << std::endl;
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
                            std::cerr << std::endl
                                      << command << ": error parsing ICC profile data from file \""
                                      << inputFileNameIterator->filename()
                                      << "\"" << enblend::optional_layer_name(layer, layers) << std::endl;
                            inputFileNameIterator->unroll_trace();
                            exit(1);
                        }
                    }

                    std::cerr << std::endl << command << ": warning: input image \""
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
                              << std::endl;
                }

                if (inputInfo->width() < minDim) {
                    minDim = inputInfo->width();
                }
                if (inputInfo->height() < minDim) {
                    minDim = inputInfo->height();
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
                      << command << ": warning: Enblend needs two or more overlapping input images in order to do\n"
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

    // Switch to fine mask, if the smallest coarse mask would be less
    // than 64 pixels wide or high.
    if (minDim / 8 < 64 && CoarseMask) {
        std::cerr << command
                  << ": warning: input images to small for coarse mask; switching to fine mask"
                  << std::endl;
        CoarseMask = false;
        if (MainAlgorithm == GraphCut) {
            std::cerr << command
                      << ": warning: fine mask combined with graphcut incompatible with mask optimization;\n"
                      << command
                      <<": warning:     defaulting to no optimization."
                      << std::endl;

            OptimizeMask = false;
        }
    }

    if (MaskVectorizeDistance.value() == 0) {
        MaskVectorizeDistance.set_percentage(false);
        MaskVectorizeDistance.set_value(CoarseMask ? coarseMaskVectorizeDistance : fineMaskVectorizeDistance);
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
                          << std::endl;
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
                    std::cerr << command << ": warning: assuming sRGB profile" << std::endl;
                    InputProfile = cmsCreate_sRGBProfile();
                } else {
                    std::cerr << command << ": warning: using fallback profile \""
                              << enblend::profileDescription(FallbackProfile) << "\"" << std::endl;
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
                          << "\" to XYZ space" << std::endl;
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
                          << "\"" << std::endl;
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
                std::cerr << std::endl
                          << command
                          << ": error initializing CIECAM02 transform"
                          << std::endl;
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
                              << std::endl;
                }
            } else {
                memcpy(&white_point, cmsD50_xyY(), sizeof(cmsCIExyY));
                if (Verbose >= VERBOSE_COLOR_CONVERSION_MESSAGES) {
                    double temperature;
                    cmsTempFromWhitePoint(&temperature, &white_point);
                    std::cerr << command
                              << ": info: falling back to predefined (D50) white point at " << temperature << "K"
                              << std::endl;
                }
            }
            LabProfile = cmsCreateLab2Profile(&white_point);
            InputToLabTransform = cmsCreateTransform(InputProfile, TYPE_RGB_DBL,
                                                     LabProfile, TYPE_Lab_DBL,
                                                     RENDERING_INTENT_FOR_BLENDING,
                                                     TRANSFORMATION_FLAGS_FOR_BLENDING);
            if (!InputToLabTransform) {
                std::cerr << command << ": error building color transform from \""
                          << enblend::profileName(InputProfile)
                          << " "
                          << enblend::profileDescription(InputProfile)
                          << "\" to Lab space" << std::endl;
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
                          << "\"" << std::endl;
                exit(1);
            }
        } else {
            if (FallbackProfile != NULL) {
                std::cerr << command <<
                    ": warning: blending in RGB cube; option \"--fallback-profile\" has no effect" <<
                    std::endl;
            }
        }

        // The size of the output image.
        if (Verbose >= VERBOSE_INPUT_UNION_SIZE_MESSAGES) {
            std::cerr << command
                      << ": info: output image size: "
                      << inputUnion
                      << std::endl;
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
        } catch (vigra::StdException & e) {
            std::cerr << std::endl
                      << command
                      << ": error opening output file \""
                      << OutputFileName
                      << "\";\n"
                      << command
                      << ": "
                      << e.what()
                      << std::endl;
            exit(1);
        }

        if (!OutputPixelType.empty()) {
            pixelType = enblend::maxPixelType(pixelType, OutputPixelType);
        }
    }

    // Invoke templatized blender.
    try {
        if (isColor) {
            if      (pixelType == "UINT8")  enblend::enblendMain<vigra::RGBValue<vigra::UInt8 > >(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
#ifndef DEBUG_8BIT_ONLY
            else if (pixelType == "INT8")   enblend::enblendMain<vigra::RGBValue<vigra::Int8  > >(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
            else if (pixelType == "UINT16") enblend::enblendMain<vigra::RGBValue<vigra::UInt16> >(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
            else if (pixelType == "INT16")  enblend::enblendMain<vigra::RGBValue<vigra::Int16 > >(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
            else if (pixelType == "UINT32") enblend::enblendMain<vigra::RGBValue<vigra::UInt32> >(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
            else if (pixelType == "INT32")  enblend::enblendMain<vigra::RGBValue<vigra::Int32 > >(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
            else if (pixelType == "FLOAT")  enblend::enblendMain<vigra::RGBValue<float > >(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
            else if (pixelType == "DOUBLE") enblend::enblendMain<vigra::RGBValue<double> >(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
#endif
            else {
                std::cerr << command << ": RGB images with pixel type \""
                          << pixelType
                          << "\" are not supported"
                          << std::endl;
                exit(1);
            }
        } else {
            if      (pixelType == "UINT8")  enblend::enblendMain<vigra::UInt8 >(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
#ifndef DEBUG_8BIT_ONLY
            else if (pixelType == "INT8")   enblend::enblendMain<vigra::Int8  >(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
            else if (pixelType == "UINT16") enblend::enblendMain<vigra::UInt16>(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
            else if (pixelType == "INT16")  enblend::enblendMain<vigra::Int16 >(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
            else if (pixelType == "UINT32") enblend::enblendMain<vigra::UInt32>(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
            else if (pixelType == "INT32")  enblend::enblendMain<vigra::Int32 >(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
            else if (pixelType == "FLOAT")  enblend::enblendMain<float >(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
            else if (pixelType == "DOUBLE") enblend::enblendMain<double>(inputFileNameList, imageInfoList, outputImageInfo, inputUnion);
#endif
            else {
                std::cerr << command
                          << ": black&white images with pixel type \""
                          << pixelType
                          << "\" are not supported"
                          << std::endl;
                exit(1);
            }
        }

        for (std::list<vigra::ImageImportInfo*>::iterator i = imageInfoList.begin();
             i != imageInfoList.end();
             ++i) {
            delete *i;
        }
    } catch (std::bad_alloc& e) {
        std::cerr << std::endl
                  << command << ": out of memory\n"
                  << command << ": " << e.what()
                  << std::endl;
        exit(1);
    } catch (vigra::StdException& e) {
        std::cerr << std::endl
                  << command << ": an exception occured\n"
                  << command << ": " << e.what()
                  << std::endl;
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

#ifdef HAVE_LIBGLEW
    if (UseGPU) {
        wrapupGPU();
    }
#endif

    // Success.
    return 0;
}
