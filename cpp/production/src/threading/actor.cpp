#include <threading/actor.h>

void conditional_lock::notify_all()
{
	cv.notify_all();
}

void conditional_lock::notify_one()
{
	cv.notify_one();
}

message_handler::~message_handler()
{}

void message_handler::handle(std::unique_ptr<message_base> message)
{
	message->visit(*this);
}

void message_handler::process(message_base& message)
{

}

message_base::~message_base()
{}

bool message_base::is_termination_message()
{
	return false;
}

void message_base::visit(message_handler& handler)
{
	handler.process(*this);
}