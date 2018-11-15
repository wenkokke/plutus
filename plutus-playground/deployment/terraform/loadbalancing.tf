# Public ALB

# Security Group
resource "aws_security_group" "public_alb" {
  vpc_id = "${aws_vpc.plutus.id}"

  ## inbound (world): ICMP 3:4 "Fragmentation Needed and Don't Fragment was Set"
  ingress {
    from_port   = "3"
    to_port     = "4"
    protocol    = "ICMP"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ## inbound (world): https
  ingress {
    from_port   = "443"
    to_port     = "443"
    protocol    = "TCP"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    from_port   = "80"
    to_port     = "80"
    protocol    = "TCP"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port   = "80"
    to_port     = "80"
    protocol    = "TCP"
    cidr_blocks = ["${var.private_subnet_cidrs}"]
  }

  tags {
    Name        = "${var.project}_${var.env}_public_alb"
    Project     = "${var.project}"
    Environment = "${var.env}"
  }
}

resource "aws_alb" "plutus" {
  subnets = ["${aws_subnet.public.*.id}"]

  security_groups = ["${aws_security_group.public_alb.id}"]
  internal        = false

  tags {
    Name        = "${var.project}_${var.env}_public_alb"
    Project     = "${var.project}"
    Environment = "${var.env}"
  }

  # access_logs {
  #   bucket = "${var.s3_bucket}"
  #   prefix = "ELB_logs"
  # }
}

resource "aws_alb_listener" "playground" {
  load_balancer_arn = "${aws_alb.plutus.arn}"
  port              = "443"
  protocol          = "HTTPS"
  certificate_arn   = "${data.aws_acm_certificate.kevm_private.arn}"

  default_action {
    target_group_arn = "${aws_alb_target_group.playground.arn}"
    type             = "forward"
  }
}

resource "aws_alb_target_group" "playground" {
  port     = "80"
  protocol = "HTTP"
  vpc_id   = "${aws_vpc.plutus.id}"

  # health_check {
  #   path = "/healthcheck"
  # }
}

resource "aws_alb_listener_rule" "playground" {
  depends_on   = ["aws_alb_target_group.playground"]
  listener_arn = "${aws_alb_listener.playground.arn}"
  priority     = 100

  action {
    type             = "forward"
    target_group_arn = "${aws_alb_target_group.playground.id}"
  }

  condition {
    field  = "path-pattern"
    values = ["*"]
  }
}

resource "aws_alb_target_group_attachment" "playground_a" {
  target_group_arn = "${aws_alb_target_group.playground.arn}"
  target_id        = "${aws_instance.playground_a.id}"
  port             = "80"
}

data "aws_acm_certificate" "kevm_private" {
  domain      = "*.plutus.dev-mantis.iohkdev.io"
  statuses    = ["ISSUED"]
  most_recent = true
}

resource "aws_route53_record" "alb" {
  zone_id = "Z2YDMO25KPZVPP"
  name    = "${var.env}.plutus.dev-mantis.iohkdev.io"
  type    = "A"

  alias {
    name                   = "${aws_alb.plutus.dns_name}"
    zone_id                = "${aws_alb.plutus.zone_id}"
    evaluate_target_health = true
  }
}
