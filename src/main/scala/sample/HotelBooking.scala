package sample

import java.time.LocalDate

import scala.collection.mutable

object HotelBooking {

  object WithSideEffect {

    def book(hotelId: Int, checking: LocalDate, checkout: LocalDate,
             paymentMethod: PaymentMethod): Boolean = {

      if (checking.isAfter(checkout) || checking.isEqual(checkout))
        throw new RuntimeException("checking can not be greater or equal than checkout")

      val hotelDB = HotelProvider.hotels.find(_.id == hotelId).getOrElse(
        throw new RuntimeException(s"Hotel $hotelId doesn't exists"))

      hotelDB.book(checking, checkout, paymentMethod)
    }

    case class Hotel(id: Int, name: String, bookings: mutable.ArrayBuffer[Booking] = mutable.ArrayBuffer()) {

      def book(checking: LocalDate, checkout: LocalDate, paymentMethod: PaymentMethod): Boolean = {
        if (this.isAvailable(checking, checkout)) {
          this.bookings.+=:(Booking(checking, checkout, paymentMethod))
          true
        } else
          false
      }

      def isAvailable(checking: LocalDate, checkout: LocalDate): Boolean = {
        bookings.forall(bkg => bkg.checking.isAfter(checkout) || bkg.checkout.isBefore(checking))
      }
    }

    case class PaymentMethod(prepaid: Boolean)
    case class Booking(checking: LocalDate, checkout: LocalDate, paymentMethod: PaymentMethod)

    object HotelProvider {
      val hotels: List[Hotel] = List(Hotel(1, "Panamericano Bs As"), Hotel(2, "Globales Republica"))
    }

  }

  // with no exception but side-effect yet
  object WithNoExceptionButSideEffect {

    def book(hotelId: Int, checking: LocalDate, checkout: LocalDate,
             paymentMethod: PaymentMethod): BookingResult = {

      if (checking.isAfter(checkout) || checking.isEqual(checkout)) {
        BookingResult(hotelId, isBooked = false,
          cause = "checking can not be greater or equal than checkout")
      } else {

        val hotelDBOpt = HotelProvider.hotels.find(_.id == hotelId)

        hotelDBOpt
          .map(hotel => {
            val result = hotel.book(checking, checkout, paymentMethod)
            if (result) {
              BookingResult(hotelId, isBooked = true)
            } else {
              BookingResult(hotelId, isBooked = false,
                cause = s"Hotel is not available for the $checking and $checkout required")
            }
          })
          .getOrElse(BookingResult(hotelId, isBooked = false, cause = s"Hotel $hotelId doesn't exists"))
      }

    }

    case class Hotel(id: Int, name: String, bookings: mutable.ArrayBuffer[Booking] = mutable.ArrayBuffer()) {

      def book(checking: LocalDate, checkout: LocalDate, paymentMethod: PaymentMethod): Boolean = {
        if (this.isAvailable(checking, checkout)) {
          this.bookings.+=:(Booking(checking, checkout, paymentMethod))
          true
        } else
          false
      }

      def isAvailable(checking: LocalDate, checkout: LocalDate): Boolean = {
        bookings.forall(bkg => bkg.checking.isAfter(checkout) || bkg.checkout.isBefore(checking))
      }
    }

    case class PaymentMethod(prepaid: Boolean)
    case class Booking(checking: LocalDate, checkout: LocalDate, paymentMethod: PaymentMethod)

    object HotelProvider {
      val hotels: List[Hotel] = List(Hotel(1, "Panamericano Bs As"), Hotel(2, "Globales Republica"))
    }

    case class BookingResult(hotelId: Int, isBooked: Boolean, cause: String = "")

  }

  // with no side-effect
  object WithNoSideEffect {

    // Put external object as argument
    def book(hotels: List[Hotel])(hotelId: Int, checking: LocalDate, checkout: LocalDate,
             paymentMethod: PaymentMethod): BookingResult = {

      if (checking.isAfter(checkout) || checking.isEqual(checkout)) {
        BookingResult(hotelId, isBooked = false,
          cause = "checking can not be greater or equal than checkout", updatedHotels = hotels)
      } else {
        hotels
          .find(_.id == hotelId)
          .map(hotel => bookHotel(hotels, hotelId, checking, checkout, paymentMethod, hotel))
          .getOrElse(BookingResult(hotelId, isBooked = false,
            cause = s"Hotel $hotelId doesn't exists", updatedHotels = hotels))
      }

    }

    private def bookHotel(hotels: List[Hotel], hotelId: Int, checking: LocalDate, checkout: LocalDate, paymentMethod: PaymentMethod, hotel: Hotel) = {

      val (updatedHotel, result) = hotel.book(checking, checkout, paymentMethod)
      if (result) {
        val updatedHotels = hotels.map(h => if (h == hotel) updatedHotel else h)
        BookingResult(hotelId, isBooked = true, updatedHotels = updatedHotels)
      } else {
        BookingResult(hotelId, isBooked = false,
          cause = s"Hotel is not available for the $checking and $checkout required", updatedHotels = hotels)
      }

    }

    case class Hotel(id: Int, name: String, bookings: List[Booking] = Nil) {

      def book(checking: LocalDate, checkout: LocalDate, paymentMethod: PaymentMethod): (Hotel, Boolean) = {
        if (this.isAvailable(checking, checkout)) {
          val newBookings = Booking(checking, checkout, paymentMethod) +: this.bookings
          (this.copy(bookings = newBookings), true)
        } else
          (this, false)
      }

      def isAvailable(checking: LocalDate, checkout: LocalDate): Boolean = {
        bookings.forall(bkg => bkg.checking.isAfter(checkout) || bkg.checkout.isBefore(checking))
      }
    }

    case class PaymentMethod(prepaid: Boolean)
    case class Booking(checking: LocalDate, checkout: LocalDate, paymentMethod: PaymentMethod)

    case class BookingResult(hotelId: Int, isBooked: Boolean, cause: String = "", updatedHotels: List[Hotel])

  }
}
