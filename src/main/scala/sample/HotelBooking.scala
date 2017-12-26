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

  // with no side-effect and no imperative approach
  object WithNoSideEffectAndNoImperativeApproach {

    case class HotelRequired(checking: LocalDate, checkout: LocalDate, paymentMethod: PaymentMethod) {
      assert(!(checking.isAfter(checkout) || checking.isEqual(checkout)),
        s"checking can not be greater or equal than checkout: checkin: $checking, checkout: $checkout")
    }

    trait BookingProgram {

      def bookHotel(hotelRequired: HotelRequired)(hotel: Hotel): BookingResult = {
        hotel.book(hotelRequired.checking, hotelRequired.checkout, hotelRequired.paymentMethod)
          .map(hotel => BookingResult(Some(hotel), isBooked = true))
          .getOrElse(BookingResult(None, isBooked = false,
            cause = s"Hotel is not available for the ${hotelRequired.checking} and ${hotelRequired.checkout} required"))
      }
    }

    case class Hotel(id: Int, name: String, bookings: List[Booking] = Nil) {

      def book(checking: LocalDate, checkout: LocalDate, paymentMethod: PaymentMethod): Option[Hotel] = {
        if (this.isAvailable(checking, checkout)) {
          val newBookings = Booking(checking, checkout, paymentMethod) +: this.bookings
          Some(this.copy(bookings = newBookings))
        } else
          None
      }

      def isAvailable(checking: LocalDate, checkout: LocalDate): Boolean = {
        bookings.forall(bkg => bkg.checking.isAfter(checkout) || bkg.checkout.isBefore(checking))
      }
    }

    case class PaymentMethod(prepaid: Boolean)
    case class Booking(checking: LocalDate, checkout: LocalDate, paymentMethod: PaymentMethod)

    case class BookingResult(hotel: Option[Hotel], isBooked: Boolean, cause: String = "") {

      def flatMap(f: Hotel => BookingResult): BookingResult = {
        if (isBooked) {
          val lastHotel = hotel.getOrElse(
            throw new RuntimeException(s"Impossible case: isBooked=True and hotel=None: $this"))
          f(lastHotel)
        } else this
      }
    }

  }
}

object BookingRunner extends App {

  import HotelBooking.WithNoSideEffectAndNoImperativeApproach._

  run()

  def run(): Unit = {
    val bookingProgram = new BookingProgram {}
    val hotel = Hotel(1, "Panamericano Bs As")
    val bookingResult = bookingProgram
      .bookHotel(HotelRequired(LocalDate.now(), LocalDate.now().plusDays(2), PaymentMethod(prepaid = false)))(hotel)
      .flatMap(bookingProgram.bookHotel(HotelRequired(LocalDate.now().plusDays(3), LocalDate.now().plusDays(5), PaymentMethod(prepaid = true))))
      .flatMap(bookingProgram.bookHotel(HotelRequired(LocalDate.now().plusDays(6), LocalDate.now().plusDays(9), PaymentMethod(prepaid = true))))

    println(s"Finish with: $bookingResult")
  }

}
