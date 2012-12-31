/*
 * Copyright (C) 2012 Christoph L. Spiel
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


#ifndef IMPEXALPHA_HXX
#define IMPEXALPHA_HXX


#include <vigra/imageinfo.hxx>
#include <vigra/impex.hxx>


namespace vigra_ext
{
    namespace detail
    {
        typedef enum
        {
            UNSIGNED_INT_8,
            UNSIGNED_INT_16,
            UNSIGNED_INT_32,
            SIGNED_INT_16,
            SIGNED_INT_32,
            FLOAT_32,
            FLOAT_64
        } pixel_t;


        inline static pixel_t
        pixel_t_of_string(const std::string& pixel_type)
        {
            if (pixel_type == "UINT8")
            {
                return UNSIGNED_INT_8;
            }
            else if (pixel_type == "UINT16")
            {
                return UNSIGNED_INT_16;
            }
            else if (pixel_type == "UINT32")
            {
                return UNSIGNED_INT_32;
            }
            else if (pixel_type == "INT16")
            {
                return SIGNED_INT_16;
            }
            else if (pixel_type == "INT32")
            {
                return SIGNED_INT_32;
            }
            else if (pixel_type == "FLOAT")
            {
                return FLOAT_32;
            }
            else if (pixel_type == "DOUBLE")
            {
                return FLOAT_64;
            }
            else
            {
                vigra_fail("vigra_ext::detail::pixel_t_of_string: unknown pixel type");
                return UNSIGNED_INT_8; // NOT REACHED
            }
        }


        template <class ValueType,
                  class ImageIterator, class ImageAccessor,
                  class AlphaIterator, class AlphaAccessor>
        inline static void
        read_image_band_and_alpha(vigra::Decoder* decoder,
                                  ImageIterator image_iterator, ImageAccessor image_accessor,
                                  AlphaIterator alpha_iterator, AlphaAccessor alpha_accessor)
        {
            typedef typename ImageIterator::row_iterator ImageRowIterator;
            typedef typename AlphaIterator::row_iterator AlphaRowIterator;

            vigra_precondition(decoder->getNumExtraBands() == 1,
                               "vigra_ext::detail::read_image_band_and_alpha: expecting exactly one alpha band");
            vigra_precondition(decoder->getNumBands() - decoder->getNumExtraBands() == 1,
                               "vigra_ext::detail::read_image_band_and_alpha: expecting exactly one image band");

            const unsigned width(decoder->getWidth());
            const unsigned height(decoder->getHeight());
            const unsigned offset(decoder->getOffset());

            for (unsigned y = 0U; y != height; ++y)
            {
                decoder->nextScanline();

                const ValueType* scanline0 = static_cast<const ValueType*>(decoder->currentScanlineOfBand(0));
                const ValueType* scanline1 = static_cast<const ValueType*>(decoder->currentScanlineOfBand(1));

                ImageRowIterator is(image_iterator.rowIterator());
                const ImageRowIterator is_end(is + width);
                AlphaRowIterator as(alpha_iterator.rowIterator());

                while (is != is_end)
                {
                    image_accessor.set(*scanline0, is);
                    scanline0 += offset;
                    ++is;

                    alpha_accessor.set(*scanline1, as);
                    scanline1 += offset;
                    ++as;
                }

                ++image_iterator.y;
                ++alpha_iterator.y;
            }
        }


        template <class ValueType,
                  class ImageIterator, class ImageAccessor,
                  class AlphaIterator, class AlphaAccessor>
        inline static void
        read_image_bands_and_alpha(vigra::Decoder* decoder,
                                   ImageIterator image_iterator, ImageAccessor image_accessor,
                                   AlphaIterator alpha_iterator, AlphaAccessor alpha_accessor)
        {
            typedef typename ImageIterator::row_iterator ImageRowIterator;
            typedef typename AlphaIterator::row_iterator AlphaRowIterator;

            vigra_precondition(decoder->getNumExtraBands() == 1,
                               "vigra_ext::detail::read_image_bands_and_alpha: expecting exactly one alpha band");
            vigra_precondition(decoder->getNumBands() - decoder->getNumExtraBands() == 3,
                               "vigra_ext::detail::read_image_bands_and_alpha: expecting exactly three image bands");
            vigra_precondition(image_accessor.size(image_iterator) == 3,
                               "vigra_ext::detail::read_image_bands_and_alpha: number of bands in file and destination image differ");

            const unsigned width(decoder->getWidth());
            const unsigned height(decoder->getHeight());
            const unsigned offset(decoder->getOffset());

            for (unsigned y = 0U; y != height; ++y)
            {
                decoder->nextScanline();

                const ValueType* scanline0 = static_cast<const ValueType*>(decoder->currentScanlineOfBand(0));
                const ValueType* scanline1 = static_cast<const ValueType*>(decoder->currentScanlineOfBand(1));
                const ValueType* scanline2 = static_cast<const ValueType*>(decoder->currentScanlineOfBand(2));
                const ValueType* scanline3 = static_cast<const ValueType*>(decoder->currentScanlineOfBand(3));

                ImageRowIterator is(image_iterator.rowIterator());
                const ImageRowIterator is_end(is + width);
                AlphaRowIterator as(alpha_iterator.rowIterator());

                while (is != is_end)
                {
                    image_accessor.setComponent(*scanline0, is, 0);
                    scanline0 += offset;
                    image_accessor.setComponent(*scanline1, is, 1);
                    scanline1 += offset;
                    image_accessor.setComponent(*scanline2, is, 2);
                    scanline2 += offset;
                    ++is;

                    alpha_accessor.set(*scanline3, as);
                    scanline3 += offset;
                    ++as;
                }

                ++image_iterator.y;
                ++alpha_iterator.y;
            }
        }


        template <class ImageIterator, class ImageAccessor,
                  class AlphaIterator, class AlphaAccessor>
        inline static void
        importImageAlpha(const vigra::ImageImportInfo& import_info,
                         ImageIterator image_iterator, ImageAccessor image_accessor,
                         AlphaIterator alpha_iterator, AlphaAccessor alpha_accessor,
                         vigra::VigraTrueType)
        {
            std::auto_ptr<vigra::Decoder> decoder(vigra::decoder(import_info));

            switch (pixel_t_of_string(decoder->getPixelType()))
            {
            case UNSIGNED_INT_8:
                read_image_band_and_alpha<vigra::UInt8>(decoder.get(),
                                                        image_iterator, image_accessor,
                                                        alpha_iterator, alpha_accessor);
                break;
            case UNSIGNED_INT_16:
                read_image_band_and_alpha<vigra::UInt16>(decoder.get(),
                                                         image_iterator, image_accessor,
                                                         alpha_iterator, alpha_accessor);
                break;
            case UNSIGNED_INT_32:
                read_image_band_and_alpha<vigra::UInt32>(decoder.get(),
                                                         image_iterator, image_accessor,
                                                         alpha_iterator, alpha_accessor);
                break;
            case SIGNED_INT_16:
                read_image_band_and_alpha<vigra::Int16>(decoder.get(),
                                                        image_iterator, image_accessor,
                                                        alpha_iterator, alpha_accessor);
                break;
            case SIGNED_INT_32:
                read_image_band_and_alpha<vigra::Int32>(decoder.get(),
                                                        image_iterator, image_accessor,
                                                        alpha_iterator, alpha_accessor);
                break;
            case FLOAT_32:
                read_image_band_and_alpha<float>(decoder.get(),
                                                 image_iterator, image_accessor,
                                                 alpha_iterator, alpha_accessor);
                break;
            case FLOAT_64:
                read_image_band_and_alpha<double>(decoder.get(),
                                                  image_iterator, image_accessor,
                                                  alpha_iterator, alpha_accessor);
                break;
            default:
                vigra_fail("vigra_ext::detail::importImageAlpha<scalar>: not reached");
            }

            decoder->close();
        }


        template <class ImageIterator, class ImageAccessor,
                  class AlphaIterator, class AlphaAccessor>
        inline static void
        importImageAlpha(const vigra::ImageImportInfo& import_info,
                         ImageIterator image_iterator, ImageAccessor image_accessor,
                         AlphaIterator alpha_iterator, AlphaAccessor alpha_accessor,
                         vigra::VigraFalseType)
        {
            std::auto_ptr<vigra::Decoder> decoder(vigra::decoder(import_info));

            switch (pixel_t_of_string(decoder->getPixelType()))
            {
            case UNSIGNED_INT_8:
                read_image_bands_and_alpha<vigra::UInt8>(decoder.get(),
                                                         image_iterator, image_accessor,
                                                         alpha_iterator, alpha_accessor);
                break;
            case UNSIGNED_INT_16:
                read_image_bands_and_alpha<vigra::UInt16>(decoder.get(),
                                                          image_iterator, image_accessor,
                                                          alpha_iterator, alpha_accessor);
                break;
            case UNSIGNED_INT_32:
                read_image_bands_and_alpha<vigra::UInt32>(decoder.get(),
                                                          image_iterator, image_accessor,
                                                          alpha_iterator, alpha_accessor);
                break;
            case SIGNED_INT_16:
                read_image_bands_and_alpha<vigra::Int16>(decoder.get(),
                                                         image_iterator, image_accessor,
                                                         alpha_iterator, alpha_accessor);
                break;
            case SIGNED_INT_32:
                read_image_bands_and_alpha<vigra::Int32>(decoder.get(),
                                                         image_iterator, image_accessor,
                                                         alpha_iterator, alpha_accessor);
                break;
            case FLOAT_32:
                read_image_bands_and_alpha<float>(decoder.get(),
                                                  image_iterator, image_accessor,
                                                  alpha_iterator, alpha_accessor);
                break;
            case FLOAT_64:
                read_image_bands_and_alpha<double>(decoder.get(),
                                                   image_iterator, image_accessor,
                                                   alpha_iterator, alpha_accessor);
                break;
            default:
                vigra_fail("vigra_ext::detail::importImageAlpha<non-scalar>: not reached");
            }

            decoder->close();
        }
    } // end namespace detail


    template <class ImageIterator, class ImageAccessor,
              class AlphaIterator, class AlphaAccessor>
    inline void
    importImageAlpha(const vigra::ImageImportInfo& import_info,
                     ImageIterator image_iterator, ImageAccessor image_accessor,
                     AlphaIterator alpha_iterator, AlphaAccessor alpha_accessor)
    {
        typedef typename ImageAccessor::value_type ImageValueType;
        typedef typename vigra::NumericTraits<ImageValueType>::isScalar is_scalar;

        detail::importImageAlpha(import_info,
                                 image_iterator, image_accessor,
                                 alpha_iterator, alpha_accessor,
                                 is_scalar());
    }


    template <class ImageIterator, class ImageAccessor,
              class AlphaIterator, class AlphaAccessor>
    inline void
    importImageAlpha(const vigra::ImageImportInfo& import_info,
                     const vigra::pair<ImageIterator, ImageAccessor>& image,
                     const vigra::pair<AlphaIterator, AlphaAccessor>& alpha)
    {
        importImageAlpha(import_info,
                         image.first, image.second,
                         alpha.first, alpha.second);
    }


    namespace detail
    {
        template<class ValueType,
                 class ImageIterator, class ImageAccessor,
                 class AlphaIterator, class AlphaAccessor>
        inline static void
        write_image_band_and_alpha(vigra::Encoder* encoder,
                                   ImageIterator image_upper_left, ImageIterator image_lower_right, ImageAccessor image_accessor,
                                   AlphaIterator alpha_upper_left, AlphaAccessor alpha_accessor)
        {
            typedef typename ImageIterator::row_iterator ImageRowIterator;
            typedef typename ImageAccessor::value_type ImageValueType;

            typedef typename AlphaIterator::row_iterator AlphaRowIterator;
            typedef typename AlphaAccessor::value_type AlphaValueType;

            vigra_precondition(image_lower_right.x >= image_upper_left.x,
                               "vigra_ext::detail::write_image_band_and_alpha: negative width");
            vigra_precondition(image_lower_right.y >= image_upper_left.y,
                               "vigra_ext::detail::write_image_band_and_alpha: negative height");

            const unsigned width(static_cast<unsigned>(image_lower_right.x - image_upper_left.x));
            const unsigned height(static_cast<unsigned>(image_lower_right.y - image_upper_left.y));

            encoder->setWidth(width);
            encoder->setHeight(height);
            encoder->setNumBands(1 + 1);
            encoder->finalizeSettings();

            const unsigned offset(encoder->getOffset()); // correct offset only _after_ finalizeSettings()

            // IMPLEMENTATION NOTE: We avoid calling the default constructor
            // to allow classes ImageIterator and AlphaIterator that do not
            // define one.
            ImageIterator image_iterator(image_upper_left);
            AlphaIterator alpha_iterator(alpha_upper_left);

            for (unsigned y = 0U; y != height; ++y)
            {
                ValueType* scanline0 = static_cast<ValueType*>(encoder->currentScanlineOfBand(0));
                ValueType* scanline1 = static_cast<ValueType*>(encoder->currentScanlineOfBand(1));

                ImageRowIterator is(image_iterator.rowIterator());
                const ImageRowIterator is_end(is + width);
                AlphaRowIterator as(alpha_iterator.rowIterator());

                while (is != is_end)
                {
                    *scanline0 = vigra::detail::RequiresExplicitCast<ValueType>::cast(image_accessor(is));
                    scanline0 += offset;
                    ++is;

                    *scanline1 = vigra::detail::RequiresExplicitCast<ValueType>::cast(alpha_accessor(as));
                    scanline1 += offset;
                    ++as;
                }

                encoder->nextScanline();

                ++image_iterator.y;
                ++alpha_iterator.y;
            }
        }


        template<class ValueType,
                 class ImageIterator, class ImageAccessor,
                 class AlphaIterator, class AlphaAccessor>
        inline static void
        write_image_bands_and_alpha(vigra::Encoder* encoder,
                                    ImageIterator image_upper_left, ImageIterator image_lower_right, ImageAccessor image_accessor,
                                    AlphaIterator alpha_upper_left, AlphaAccessor alpha_accessor)
        {
            typedef typename ImageIterator::row_iterator ImageRowIterator;
            typedef typename AlphaIterator::row_iterator AlphaRowIterator;

            vigra_precondition(image_lower_right.x >= image_upper_left.x,
                               "vigra_ext::detail::write_image_bands_and_alpha: negative width");
            vigra_precondition(image_lower_right.y >= image_upper_left.y,
                               "vigra_ext::detail::write_image_bands_and_alpha: negative height");
            vigra_precondition(image_accessor.size(image_upper_left) == 3,
                               "vigra_ext::detail::write_image_bands_and_alpha: number of bands in file and source image differ");

            const unsigned width(static_cast<unsigned>(image_lower_right.x - image_upper_left.x));
            const unsigned height(static_cast<unsigned>(image_lower_right.y - image_upper_left.y));

            encoder->setWidth(width);
            encoder->setHeight(height);
            encoder->setNumBands(3 + 1);
            encoder->finalizeSettings();

            const unsigned offset(encoder->getOffset()); // correct offset only _after_ finalizeSettings()

            // IMPLEMENTATION NOTE: We avoid calling the default constructor
            // to allow classes ImageIterator and AlphaIterator that do not
            // define one.
            ImageIterator image_iterator(image_upper_left);
            AlphaIterator alpha_iterator(alpha_upper_left);

            for (unsigned y = 0U; y != height; ++y)
            {
                ValueType* scanline0 = static_cast<ValueType*>(encoder->currentScanlineOfBand(0));
                ValueType* scanline1 = static_cast<ValueType*>(encoder->currentScanlineOfBand(1));
                ValueType* scanline2 = static_cast<ValueType*>(encoder->currentScanlineOfBand(2));
                ValueType* scanline3 = static_cast<ValueType*>(encoder->currentScanlineOfBand(3));

                ImageRowIterator is(image_iterator.rowIterator());
                const ImageRowIterator is_end(is + width);
                AlphaRowIterator as(alpha_iterator.rowIterator());

                while (is != is_end)
                {
                    *scanline0 = vigra::detail::RequiresExplicitCast<ValueType>::cast(image_accessor.getComponent(is, 0));
                    scanline0 += offset;
                    *scanline1 = vigra::detail::RequiresExplicitCast<ValueType>::cast(image_accessor.getComponent(is, 1));
                    scanline1 += offset;
                    *scanline2 = vigra::detail::RequiresExplicitCast<ValueType>::cast(image_accessor.getComponent(is, 2));
                    scanline2 += offset;
                    ++is;

                    *scanline3 = vigra::detail::RequiresExplicitCast<ValueType>::cast(alpha_accessor(as));
                    scanline3 += offset;
                    ++as;
                }

                encoder->nextScanline();

                ++image_iterator.y;
                ++alpha_iterator.y;
            }
        }


        template <class ImageIterator, class ImageAccessor,
                  class AlphaIterator, class AlphaAccessor>
        inline static void
        exportImageAlpha(ImageIterator image_upper_left, ImageIterator image_lower_right, ImageAccessor image_accessor,
                         AlphaIterator alpha_upper_left, AlphaAccessor alpha_accessor,
                         const vigra::ImageExportInfo& export_info,
                         vigra::VigraTrueType)
        {
            const std::string pixel_type(export_info.getPixelType());
            std::auto_ptr<vigra::Encoder> encoder(vigra::encoder(export_info));

            encoder->setPixelType(pixel_type);

            switch (pixel_t_of_string(pixel_type))
            {
            case UNSIGNED_INT_8:
                write_image_band_and_alpha<vigra::UInt8>(encoder.get(),
                                                         image_upper_left, image_lower_right, image_accessor,
                                                         alpha_upper_left, alpha_accessor);
                break;
            case UNSIGNED_INT_16:
                write_image_band_and_alpha<vigra::UInt16>(encoder.get(),
                                                          image_upper_left, image_lower_right, image_accessor,
                                                          alpha_upper_left, alpha_accessor);
                break;
            case UNSIGNED_INT_32:
                write_image_band_and_alpha<vigra::UInt32>(encoder.get(),
                                                          image_upper_left, image_lower_right, image_accessor,
                                                          alpha_upper_left, alpha_accessor);
                break;
            case SIGNED_INT_16:
                write_image_band_and_alpha<vigra::Int16>(encoder.get(),
                                                         image_upper_left, image_lower_right, image_accessor,
                                                         alpha_upper_left, alpha_accessor);
                break;
            case SIGNED_INT_32:
                write_image_band_and_alpha<vigra::Int32>(encoder.get(),
                                                         image_upper_left, image_lower_right, image_accessor,
                                                         alpha_upper_left, alpha_accessor);
                break;
            case FLOAT_32:
                write_image_band_and_alpha<float>(encoder.get(),
                                                  image_upper_left, image_lower_right, image_accessor,
                                                  alpha_upper_left, alpha_accessor);
                break;
            case FLOAT_64:
                write_image_band_and_alpha<double>(encoder.get(),
                                                   image_upper_left, image_lower_right, image_accessor,
                                                   alpha_upper_left, alpha_accessor);
                break;
            default:
                vigra_fail("vigra_ext::detail::exportImageAlpha<scalar>: not reached");
            }

            encoder->close();
        }


        template <class ImageIterator, class ImageAccessor,
                  class AlphaIterator, class AlphaAccessor>
        inline static void
        exportImageAlpha(ImageIterator image_upper_left, ImageIterator image_lower_right, ImageAccessor image_accessor,
                         AlphaIterator alpha_upper_left, AlphaAccessor alpha_accessor,
                         const vigra::ImageExportInfo& export_info,
                         vigra::VigraFalseType)
        {
            const std::string pixel_type(export_info.getPixelType());
            std::auto_ptr<vigra::Encoder> encoder(vigra::encoder(export_info));

            encoder->setPixelType(pixel_type);

            switch (pixel_t_of_string(pixel_type))
            {
            case UNSIGNED_INT_8:
                write_image_bands_and_alpha<vigra::UInt8>(encoder.get(),
                                                          image_upper_left, image_lower_right, image_accessor,
                                                          alpha_upper_left, alpha_accessor);
                break;
            case UNSIGNED_INT_16:
                write_image_bands_and_alpha<vigra::UInt16>(encoder.get(),
                                                           image_upper_left, image_lower_right, image_accessor,
                                                           alpha_upper_left, alpha_accessor);
                break;
            case UNSIGNED_INT_32:
                write_image_bands_and_alpha<vigra::UInt32>(encoder.get(),
                                                           image_upper_left, image_lower_right, image_accessor,
                                                           alpha_upper_left, alpha_accessor);
                break;
            case SIGNED_INT_16:
                write_image_bands_and_alpha<vigra::Int16>(encoder.get(),
                                                          image_upper_left, image_lower_right, image_accessor,
                                                          alpha_upper_left, alpha_accessor);
                break;
            case SIGNED_INT_32:
                write_image_bands_and_alpha<vigra::Int32>(encoder.get(),
                                                          image_upper_left, image_lower_right, image_accessor,
                                                          alpha_upper_left, alpha_accessor);
                break;
            case FLOAT_32:
                write_image_bands_and_alpha<float>(encoder.get(),
                                                   image_upper_left, image_lower_right, image_accessor,
                                                   alpha_upper_left, alpha_accessor);
                break;
            case FLOAT_64:
                write_image_bands_and_alpha<double>(encoder.get(),
                                                    image_upper_left, image_lower_right, image_accessor,
                                                    alpha_upper_left, alpha_accessor);
                break;
            default:
                vigra_fail("vigra_ext::detail::exportImageAlpha<non-scalar>: not reached");
            }

            encoder->close();
        }
    } // end namespace detail


    template <class ImageIterator, class ImageAccessor,
              class AlphaIterator, class AlphaAccessor>
    inline void
    exportImageAlpha(ImageIterator image_upper_left, ImageIterator image_lower_right, ImageAccessor image_accessor,
                     AlphaIterator alpha_upper_left, AlphaAccessor alpha_accessor,
                     const vigra::ImageExportInfo& export_info)
    {
        typedef typename ImageAccessor::value_type ImageValueType;
        typedef typename vigra::NumericTraits<ImageValueType>::isScalar is_scalar;

        try
        {
            detail::exportImageAlpha(image_upper_left, image_lower_right, image_accessor,
                                     alpha_upper_left, alpha_accessor,
                                     export_info,
                                     is_scalar());
        }
        catch (vigra::Encoder::TIFFCompressionException&)
        {
            const_cast<vigra::ImageExportInfo&>(export_info).setCompression("");
            detail::exportImageAlpha(image_upper_left, image_lower_right, image_accessor,
                                     alpha_upper_left, alpha_accessor,
                                     export_info,
                                     is_scalar());
        }
    }


    template <class ImageIterator, class ImageAccessor,
              class AlphaIterator, class AlphaAccessor>
    inline void
    exportImageAlpha(const vigra::triple<ImageIterator, ImageIterator, ImageAccessor>& image,
                     const vigra::pair<AlphaIterator, AlphaAccessor>& alpha,
                     const vigra::ImageExportInfo& export_info)
    {
        exportImageAlpha(image.first, image.second, image.third,
                         alpha.first, alpha.second,
                         export_info);
    }
} // end namespace vigra_ext


#endif // IMPEXALPHA_HXX
